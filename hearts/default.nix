{ mkDerivation, base, mtl, random, stdenv, transformers, polysemy, polysemy-plugin, free, cond, aeson, containers, servant, servant-server, servant-options, wai, wai-cors, warp, monad-par }:
mkDerivation {
  pname = "hearts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl random transformers polysemy polysemy-plugin free cond aeson containers servant servant-server servant-options wai wai-cors warp monad-par ];
  homepage = "https://github.com/active-group/funar/";
  license = stdenv.lib.licenses.bsd3;
}
