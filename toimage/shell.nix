{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "recipe-toimage";

  nativeBuildInputs = with pkgs.buildPackages; [
    bacon
    cairo
    cargo
    clippy
    inriafonts
    pango
    pkg-config
    rust-analyzer
    rustfmt
  ];
}
