with import <nixpkgs> {};

stdenv.mkDerivation
{ name = "commando";
  buildInputs =
    [ swiProlog
      scala
      sbt
      jdk
      # haskellPackages.purescript
      # nodejs
      # nodePackages.bower
    ] ++ (with python3.pkgs;
    [
      python
      pyaml
      numpy
      pip
      pep8
    ]) ++ (with ocaml-ng.ocamlPackages_4_05;
    [
      ocaml
      ocamlbuild
      merlin
      findlib
    ]);
  shellHook = ''
    export PYTHONPATH=$HOME/.local/lib/python3.6/site-packages/:$PYTHONPATH
  '';
}
