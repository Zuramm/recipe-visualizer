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
    sccache
  ];

  shellHook = ''
    export RUSTC_WRAPPER=${pkgs.buildPackages.sccache}/bin/sccache
  '';
}
