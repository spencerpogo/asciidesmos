{
  # https://www.tomhoule.com/2021/building-rust-wasm-with-nix-flakes/
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ rust-overlay.overlay ];
        pkgs = import nixpkgs { inherit overlays system; };
        rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      in { devShell = pkgs.mkShell { packages = [ pkgs.wasm-pack rust ]; }; });
}
