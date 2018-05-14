with import <nixpkgs> {};

stdenv.mkDerivation
{ name = "commando";
  buildInputs =
    [ scala
      sbt
      jdk
    ];
}
