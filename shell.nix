with import <nixpkgs> {};

stdenv.mkDerivation
{ name = "commando";
  buildInputs =
    [ scala
      sbt
      jdk
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
      merlin
      findlib
    ]);
  shellHook = ''
    export PYTHONPATH=$HOME/.local/lib/python3.6/site-packages/:$PYTHONPATH
  '';
}
