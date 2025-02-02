{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "aoc2024";
          version = "n/a";
          src = ./.;
          buildInputs = with pkgs.ocamlPackages; [
            angstrom
            cmdliner
            core_kernel
            hex_encode
            ocamlgraph
            ppx_jane
          ];
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocamlformat_0_27_0
          ];
        };
      });
}
