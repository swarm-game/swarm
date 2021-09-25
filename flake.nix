{
  description = "https://github.com/byorgey/swarm";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          swarmProject =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                # hlint = {};
                # haskell-language-server = {};
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.swarmProject.flake {};
    in flake // {
      # `nix build .`
      defaultPackage = flake.packages."swarm:exe:swarm";

      # `nix run`
      defaultApp = flake.apps."swarm:exe:swarm";
    });
}
