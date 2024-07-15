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
    sccache
  ];

  shellHook = ''
    export RUSTC_WRAPPER=${pkgs.buildPackages.sccache}/bin/sccache
  '';
}
