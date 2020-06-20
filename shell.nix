let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = [
    ];

    LD_LIBRARY_PATH = "${pkgs.glibc}/lib";
  }
