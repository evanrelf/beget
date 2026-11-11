{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beget.Registry
  ( discoverInstances
  )
where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import SomeDictOf (SomeDict, SomeDictOf (..))
import Type.Reflection (Typeable, typeRep, typeRepTyCon, tyConName)

discoverInstances :: forall (c :: _ -> Constraint). Typeable c => TH.Code TH.Q [SomeDict c]
discoverInstances = TH.liftCode do
  let className = tyConName (typeRepTyCon (typeRep @c))
  -- TODO: Handle instances with multiple type parameters?
  instanceDecs <- TH.reifyInstances (TH.mkName className) [TH.VarT (TH.mkName "a")]
  let listTE :: [TH.TExp a] -> TH.TExp [a]
      listTE = TH.TExp . TH.ListE . map TH.unType
  dicts <- fmap listTE $ traverse decToDict instanceDecs
  TH.examineCode [|| concat $$(TH.liftCode $ pure dicts) ||]

decToDict :: forall k (c :: k -> Constraint). TH.InstanceDec -> TH.Q (TH.TExp [SomeDict c])
decToDict = \case
  TH.InstanceD _mOverlap cxt typ _decs ->
    if null cxt then do
      let stripSig = \case
            TH.SigT a _ -> a
            x -> x
      let t = case typ of
            TH.AppT _ t' -> stripSig t'
            _ -> t
      let proxy = [| Proxy :: Proxy $(pure t) |]
      TH.unsafeTExpCoerce [| [ SomeDictOf $proxy ] |]
    else
      -- TODO: Handle instances with context
      TH.examineCode [|| [] ||]
  _ -> do
    TH.reportWarning "discoverInstances called on 'reifyInstances' somehow returned something that wasn't a type class instance."
    TH.examineCode [|| [] ||]
