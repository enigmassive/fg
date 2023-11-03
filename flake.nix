{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    naersk = {
      url = "github:nix-community/naersk";
    };
    fenix = {
      url = "github:nix-community/fenix";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
    fenix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (fenix.packages.${system}.stable) toolchain;

        nativeBuildInputs = with pkgs; [
          pkg-config
        ];
        buildInputs = with pkgs; [
          openssl
        ];
      in {
        defaultPackage =
          (naersk.lib.${system}.override {
            cargo = toolchain;
            rustc = toolchain;
          })
          .buildPackage {
            src = ./.;
            inherit nativeBuildInputs;
            inherit buildInputs;
          };

        devShell = pkgs.mkShell {
          inherit nativeBuildInputs;
          inherit buildInputs;
        };
      }
    );
}
