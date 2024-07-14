{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "recipe-gtk";

  nativeBuildInputs = with pkgs.buildPackages; [
    bacon
    cargo
    clippy
    gtk4
    pkg-config
    rust-analyzer
    rustfmt
  ];
}
