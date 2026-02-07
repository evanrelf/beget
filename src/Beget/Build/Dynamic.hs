{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Beget.Build.Dynamic
  ( Build (..)
  , unwrapBuild
  , runBuild
  , realize

  , Task (..)
  , TaskOptions (..)
  , defaultTaskOptions

  , initBuild
  , registerTask
  , registerTaskVolatile
  , registerTaskWith
  )
where

import Beget.Build.Static qualified as Static
import Beget.Registry (discoverInstances, getInstances, registerInstances)
import Beget.Value (SomeValue (..), Value, fromSomeValue, fromSomeValue', toSomeValue)
import Codec.Serialise (Serialise)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Char (toUpper)
import Data.Coerce (Coercible, coerce)
import Data.Constraint (Class (..), Dict (..), (:-) (..))
import Data.Functor.Identity (Identity (..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Constraint, Type)
import Database.SQLite.Simple qualified as SQLite
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
import SomeDictOf (SomeDict, SomeDictOf (..))
import Type.Reflection (Typeable)
import UnliftIO (MonadUnliftIO)
import VarArgs ((:->:))

type TaskState = Static.TaskState SomeValue SomeValue

newtype Build a = Build (ReaderT TaskState IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

unwrapBuild :: Build a -> (TaskState -> IO a)
unwrapBuild (Build (ReaderT f)) = \s -> f s

runBuild :: SQLite.Connection -> Build a -> IO a
runBuild connection taskM = do
  let tasks = getTasks
  buildState <- atomically $ Static.newBuildState connection (\taskState someKey -> unwrapBuild (tasks someKey) taskState)
  taskState <- atomically $ Static.newTaskState buildState
  unwrapBuild taskM taskState

realize :: Task a => a -> TaskArgs a :->: Build (TaskResult a)
realize sing = curryN \(args :: TupleArgs (TaskArgs a)) -> do
  uncurryN (taskRealize @a (Identity sing)) args :: Build (TaskResult a)

type Task :: Type -> Constraint
class
  ( Typeable a
  , Value (TaskKey a)
  , Value (TaskValue a)
  , Coercible (TaskKey a) (TupleArgs (TaskArgs a))
  , Coercible (TaskValue a) (TaskResult a)
  , CurryN (TaskArgs a)
  )
  => Task a where
  type TaskArgs a :: [Type]

  type TaskResult a :: Type

  data TaskKey a :: Type

  data TaskValue a :: Type

  taskOptions :: TaskOptions
  taskOptions = defaultTaskOptions

  taskSing :: a

  taskBuild :: proxy a -> TaskArgs a :->: Build (TaskResult a)

instance Class (Typeable a) (Task a) where
  cls = Sub Dict

taskRealize :: Task a => proxy a -> TaskArgs a :->: Build (TaskResult a)
taskRealize proxy = curryN \args -> do
  taskState <- Build ask
  someValue <- liftIO $ Static.taskStateRealize taskState (toSomeValue (argsToKey proxy args))
  pure (valueToResult proxy (fromSomeValue' someValue))

taskHandler :: Task a => proxy a -> TaskKey a -> Build (TaskValue a, Bool)
taskHandler @a proxy key = do
  result <- uncurryN (taskBuild proxy) (keyToArgs key)
  let options = taskOptions @a
  pure (resultToValue result, options.volatile)

argsToKey :: Task a => proxy a -> TupleArgs (TaskArgs a) -> TaskKey a
argsToKey _ = coerce

keyToArgs :: Task a => TaskKey a -> TupleArgs (TaskArgs a)
keyToArgs = coerce

resultToValue :: Task a => TaskResult a -> TaskValue a
resultToValue = coerce

valueToResult :: Task a => proxy a -> TaskValue a -> TaskResult a
valueToResult _ = coerce

data TaskOptions = TaskOptions
  { volatile :: Bool
  }
  deriving stock (Lift)

defaultTaskOptions :: TaskOptions
defaultTaskOptions =
  TaskOptions
    { volatile = False
    }

-- TODO: Somehow make `Value` and `Task` qualified types so you don't have to
-- import them at the call site.
initBuild :: TH.Code TH.Q (IO ())
initBuild =
  [|| do
    let values :: [SomeDict Value]
        values = $$discoverInstances
    registerInstances values
    let tasks :: [SomeDict Task]
        tasks = $$discoverInstances
    registerInstances tasks
  ||]

registerTask :: TH.Name -> TH.Q [TH.Dec]
registerTask funName = registerTaskWith funName defaultTaskOptions

registerTaskVolatile :: TH.Name -> TH.Q [TH.Dec]
registerTaskVolatile funName =
  registerTaskWith funName defaultTaskOptions{ volatile = True }

-- TODO: Consider any task with zero arguments as volatile, overriding the
-- user's option.
registerTaskWith :: TH.Name -> TaskOptions -> TH.Q [TH.Dec]
registerTaskWith funName options = do
  info <- TH.reify funName
  typ <- case info of
    TH.VarI _ typ _ -> pure typ
    _ -> fail "Task registration expected a function"
  let (taskArgs, returnType) = argsAndResult typ
  taskResult <- case unwrapIO returnType of
    Just taskResult -> pure taskResult
    Nothing -> fail "Task function must run in 'Build' monad"
  sequence
    [ generateDataDec
    , generateInstDec taskArgs taskResult
    ]
  where
  argsAndResult :: TH.Type -> ([TH.Type], TH.Type)
  argsAndResult = go []
    where
    go args (TH.AppT (TH.AppT TH.ArrowT arg) rest) = go (arg : args) rest
    go args result = (reverse args, result)

  unwrapIO :: TH.Type -> Maybe TH.Type
  unwrapIO = \case
    (TH.AppT (TH.ConT f) a) | TH.nameBase f == "Build" -> Just a
    _ -> Nothing

  capitalizeFirst :: String -> String
  capitalizeFirst = \case
    [] -> []
    (c : cs) -> toUpper c : cs

  baseName = TH.nameBase funName
  dataName = TH.mkName (capitalizeFirst baseName)
  keyName = TH.mkName (capitalizeFirst baseName ++ "Key")
  valueName = TH.mkName (capitalizeFirst baseName ++ "Value")

  generateDataDec :: TH.Q TH.Dec
  generateDataDec =
    pure $ TH.DataD [] dataName [] Nothing [TH.NormalC dataName []] []

  generateInstDec :: [TH.Type] -> TH.Type -> TH.Q TH.Dec
  generateInstDec taskArgs taskResult = do
    let dataType = TH.ConT dataName

    -- `instance Task ...`
    instanceHead <- [t| Task $(pure dataType) |]

    -- `type TaskArgs _ = ...`
    taskArgsInst <- do
      let promote :: [TH.Type] -> TH.Type
          promote = foldr (\t ts -> TH.AppT (TH.AppT TH.PromotedConsT t) ts) TH.PromotedNilT
      lhs <- [t| TaskArgs $(pure dataType) |]
      pure $ TH.TySynInstD $ TH.TySynEqn Nothing lhs (promote taskArgs)

    -- `type TaskResult _ = ...`
    taskResultInst <- do
      lhs <- [t| TaskResult $(pure dataType) |]
      pure $ TH.TySynInstD $ TH.TySynEqn Nothing lhs taskResult

    let newtypeField typ = [(TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, typ)]

    -- `newtype TaskKey _ = ...`
    taskKeyInst <- TH.NewtypeInstD [] Nothing
      <$> [t| TaskKey $(pure dataType) |]
      <*> pure Nothing
      <*> (TH.NormalC keyName . newtypeField <$> [t| TupleArgs (TaskArgs $(pure dataType)) |])
      <*> pure
            [ TH.DerivClause (Just TH.StockStrategy) (map TH.ConT [''Generic, ''Show, ''Eq])
            , TH.DerivClause (Just TH.AnyclassStrategy) (map TH.ConT [''Serialise, ''Hashable, ''Value])
            ]

    -- `newtype TaskValue _ = ...`
    taskValueInst <- TH.NewtypeInstD [] Nothing
      <$> [t| TaskValue $(pure dataType) |]
      <*> pure Nothing
      <*> (TH.NormalC valueName . newtypeField <$> [t| TaskResult $(pure dataType) |])
      <*> pure
            [ TH.DerivClause (Just TH.StockStrategy) (map TH.ConT [''Generic, ''Show, ''Eq])
            , TH.DerivClause (Just TH.AnyclassStrategy) (map TH.ConT [''Serialise, ''Hashable, ''Value])
            ]

    -- `taskOptions = ...`
    taskOptionsFun <- TH.funD (TH.mkName "taskOptions")
      [TH.clause [] (TH.normalB (TH.lift options)) []]

    -- `taskSing = ...`
    taskSingFun <- TH.funD (TH.mkName "taskSing")
      [TH.clause [] (TH.normalB (TH.conE dataName)) []]

    -- `taskBuild = ...`
    taskBuildFun <- TH.funD (TH.mkName "taskBuild")
      [TH.clause [TH.wildP] (TH.normalB (TH.varE funName)) []]

    pure $ TH.InstanceD Nothing [] instanceHead
      [ taskArgsInst
      , taskResultInst
      , taskKeyInst
      , taskValueInst
      , taskOptionsFun
      , taskSingFun
      , taskBuildFun
      ]

data TaskHandler where
  TaskHandler :: Task a => (TaskKey a -> Build (TaskValue a, Bool)) -> TaskHandler

getTasks :: SomeValue -> Build (SomeValue, Bool)
getTasks someKey@(SomeValue t _) = do
  let instances = getInstances @Task
  let dicts = HashMap.elems instances
  let toTaskHandler (SomeDictOf @Task proxy) = TaskHandler (taskHandler proxy)
  let taskHandlers = map toTaskHandler dicts
  let tryHandler (TaskHandler handler) rest =
        case fromSomeValue someKey of
          Just key -> do
            (value, volatile) <- handler key
            pure (toSomeValue value, volatile)
          Nothing -> rest
  let fallback = error $ "No handler for task `" <> show t <> "`; `Task` instance missing from registry"
  foldr tryHandler fallback taskHandlers

type CurryN :: [Type] -> Constraint
class CurryN args where
  type TupleArgs args = (r :: Type) | r -> args
  curryN :: (TupleArgs args -> result) -> (args :->: result)
  uncurryN :: (args :->: result) -> (TupleArgs args -> result)

instance CurryN '[] where
  type TupleArgs '[] = ()
  curryN f = f ()
  uncurryN f () = f

instance CurryN '[a] where
  type TupleArgs '[a] = Identity a
  curryN f a = f (Identity a)
  uncurryN f (Identity a) = f a

instance CurryN '[a, b] where
  type TupleArgs '[a, b] = (a, b)
  curryN f a b = f (a, b)
  uncurryN f (a, b) = f a b

instance CurryN '[a, b, c] where
  type TupleArgs '[a, b, c] = (a, b, c)
  curryN f a b c = f (a, b, c)
  uncurryN f (a, b, c) = f a b c

instance CurryN '[a, b, c, d] where
  type TupleArgs '[a, b, c, d] = (a, b, c, d)
  curryN f a b c d = f (a, b, c, d)
  uncurryN f (a, b, c, d) = f a b c d
