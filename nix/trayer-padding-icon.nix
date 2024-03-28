{ stdenv }:

stdenv.mkDerivation rec {
  name = "trayer-padding-icon";
  version = "1.0";
  src = ./trayer-padding-icon.sh;  # Specify the path to your local script

  dontUnpack = true; # used since it's just one script file not an archive
  
  # Install the shell script
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/trayer-padding-icon.sh
    chmod +x $out/bin/trayer-padding-icon.sh
  '';
}
