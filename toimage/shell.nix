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
    pkg-config
    rust-analyzer
    rustfmt
  ];
}
