with import <nixpkgs> { };

pkgs.stdenv.mkDerivation {
  pname = "rofi-buku";
  version = "1.0";

  src = builtins.fetchGit {
    url = "https://github.com/knatsakis/rofi-buku";
    rev = "e483b5a94102f06627b603e70dceba91487726a0";
  };
  
  buildInputs = [ makeWrapper ];
  
  buildPhase = ''
    echo "Skipping the build phase. "
  '';
  
  installPhase = ''
    mkdir -p $out/bin
    cp config.buku $out/bin/rofi-buku.config
    cp rofi-buku $out/bin/rofi-buku
    wrapProgram $out/bin/rofi-buku \
      --prefix PATH : ${lib.makeBinPath [ bash buku gawk rofi stdenv ]}
  '';
}
