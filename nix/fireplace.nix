{ lib, stdenv, fetchgit, ncurses5 }:

stdenv.mkDerivation rec {
  pname = "fireplace";
  version = "1.0";

  src = fetchgit {
    url = "https://github.com/Wyatt915/fireplace";
    rev = "aa2070b73be9fb177007fc967b066d88a37e3408";
    sha256 = "2NUE/zaFoGwkZxgvVCYXxToiL23aVUFwFNlQzEq9GEc=";
    # sha256 = lib.fakeSha256;
  };

  buildInputs = [ ncurses5 ];  # Add dependencies needed for building, if any

  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp fireplace $out/bin
  '';

  meta = {
    description = "A cozy fireplace in your terminal";
    homepage = "https://github.com/Wyatt915/fireplace";
    license = lib.licenses.mit;  # Change the license accordingly
  };
}
