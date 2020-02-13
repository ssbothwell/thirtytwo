{ mkDerivation, base, lens, mtl, stdenv }:
mkDerivation {
  pname = "thirtytwo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens mtl ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
