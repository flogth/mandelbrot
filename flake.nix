{
  description = "A visualisation of the mandelbrot set";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        mandelbrot = pkgs.haskellPackages.developPackage { root = ./.; };

      in { packages.default = mandelbrot; });
}
