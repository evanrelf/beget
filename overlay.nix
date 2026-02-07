final: prev:

let
  sources =
    final.haskell.lib.packageSourceOverrides {
      varargs = "0.1.0.1";
    };
in
{
  haskellPackages =
    prev.haskellPackages.override (attrs: {
      overrides =
        final.lib.foldr
          final.lib.composeExtensions
          (attrs.overrides or (_: _: { }))
          [ sources ];
    });

  all-cabal-hashes =
    let
      rev = "ff6190ae97f07940b694d28dc9d7d6f119659316";
      hash = "sha256-8a4q3cM/JyPI+hhvMPeqwBv5f8Sf+k+osfc5mKy7on4=";
    in
    final.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      inherit hash;
    };
}
