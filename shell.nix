let
  pkgs = import <nixpkgs> {};
in
  pkgs.stdenv.mkDerivation {
    name = "none";
    buildInputs = with pkgs; [
      scala
      sbt
      python3
    ] ++ (with python3.pkgs; [
      pip
      pep8
      pylint
    ]);
    shellHook = ''
      export EDITOR=vim
      export VISUAL=code
    '';
  }
