{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "recipe-visualizer";

  nativeBuildInputs = with pkgs.buildPackages; [
    bacon
    cargo
    clippy
    rust-analyzer
    rustfmt
  ];
}
