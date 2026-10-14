{ lib, stdenv, fetchurl, autoPatchelfHook }:

stdenv.mkDerivation {
  pname = "plannotator-tui";
  version = "0.9.4";

  src = fetchurl {
    url = "https://github.com/plannotator/plannotator-tui/releases/download/v0.9.4/plannotator-tui-x86_64-unknown-linux-gnu";
    hash = "sha256-1U3GA8lfcQZ3vBPr5rJLLm6xDOdhV3r4qVoFAChit00=";
  };
  dontUnpack = true;
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ stdenv.cc.cc.lib ];
  installPhase = ''
    runHook preInstall
    install -Dm755 "$src" "$out/bin/plannotator-tui"
    runHook postInstall
  '';

  meta = {
    description = "Annotate Markdown in the terminal";
    homepage = "https://github.com/plannotator/plannotator-tui";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" ];
    mainProgram = "plannotator-tui";
  };
}
