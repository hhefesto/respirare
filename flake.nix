{
  description = "Nix flake for Respirare";

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays =
        [ haskellNix.overlay
          (final: prev: {
            respirare = final.haskell-nix.cabalProject {
              # If these null parameters are absent, you get a RestrictedPathError error
              # from trying to do readIfExists on cabal.project file
              cabalProjectFreeze = null;
              cabalProject = null;
              cabalProjectLocal = null;

              src = final.haskell-nix.cleanSourceHaskell {
                src = ./.;
                name = "respirare";
              };
              compiler-nix-name = "ghc884";
              pkg-def-extras = with final.haskell.lib; [];
            };
          })
        ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.respirare.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."respirare:exe:respirare-exe";

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.respirare.shellFor {
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
          ghcid = "latest";
        };
      };
    });
}
