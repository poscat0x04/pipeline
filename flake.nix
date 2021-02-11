{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = pipeline-dev.envFunc { };
            defaultPackage = pipeline;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          pipeline = hpkgs.callCabal2nix "pipeline" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit pipeline;
            pipeline-dev = addBuildTools pipeline [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
