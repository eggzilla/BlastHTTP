{ mkDerivation, base, BiobaseBlast, BiobaseFasta, bytestring
, conduit, either-unwrap, HTTP, http-conduit, hxt, mtl, network
, stdenv, transformers, zip-archive
}:
mkDerivation {
  pname = "BlastHTTP";
  version = "1.4.2";
  src = ./.;
  libraryHaskellDepends = [
    base BiobaseBlast BiobaseFasta bytestring conduit either-unwrap
    HTTP http-conduit hxt mtl network transformers zip-archive
  ];
  homepage = "https://github.com/eggzilla/BlastHTTP";
  description = "Libary to interface with the NCBI blast REST interface";
  license = stdenv.lib.licenses.gpl3;
}
