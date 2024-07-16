{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "recipe-visualizer";

  nativeBuildInputs = with pkgs.buildPackages; [
    # python
    black
    isort
    pyright
    python3

    #rust
    bacon
    cargo
    clippy
    rust-analyzer
    rustfmt
    sccache
  ];

  shellHook = ''
    export RUSTC_WRAPPER=${pkgs.buildPackages.sccache}/bin/sccache
  '';
}
