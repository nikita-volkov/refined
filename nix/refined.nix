{ mkDerivation, base, containers, exceptions, prettyprinter
, prettyprinter-ansi-terminal, stdenv, template-haskell
, transformers
}:
mkDerivation {
  pname = "refined";
  version = "0.1.2.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers exceptions prettyprinter
    prettyprinter-ansi-terminal template-haskell transformers
  ];
  testHaskellDepends = [ base containers ];
  homepage = "https://github.com/nikita-volkov/refined";
  description = "Refinement types with static and runtime checking";
  license = stdenv.lib.licenses.mit;
}
