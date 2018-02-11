{ mkDerivation, async, base, containers, foreign-store, stdenv, stm
}:
mkDerivation {
  pname = "rapid";
  version = "0.1.4";
  src = ./.;
  libraryHaskellDepends = [
    async base containers foreign-store stm
  ];
  homepage = "https://github.com/esoeylemez/rapid";
  description = "Rapid prototyping with GHCi: hot reloading of running components and reload-surviving values";
  license = stdenv.lib.licenses.bsd3;
}
