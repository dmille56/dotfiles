{ pkgs ? import <nixpkgs> { },
  exposeModels ? false
}:
let
  python = pkgs.python312;
  pythonPackages = pkgs.python312Packages;

  kokoroSrc = pkgs.fetchzip {
    url = "https://github.com/nazdridoy/kokoro-tts/archive/refs/tags/v2.3.1.tar.gz";
    # From nix build fixed-output hash mismatch (expected by fetchzip)
    sha256 = "sha256-/2sII7wbSzuP4fESx+yzMN9sackgz9Xc5jU90kQ0oI4=";
  };

  kokoroModels = pkgs.stdenv.mkDerivation {
    pname = "kokoro-tts-models";
    version = "v1.0.0";
    src = null;
    dontUnpack = true;

    installPhase = ''
      mkdir -p $out/share/kokoro-tts

      cp ${pkgs.fetchurl {
        url = "https://github.com/nazdridoy/kokoro-tts/releases/download/v1.0.0/voices-v1.0.bin";
        # nix-prefetch-url --type sha256 <url>
        sha256 = "1vfzyvr5f3c4zzs16p8q31cd3aip2vgi2xwana669rphdka655yi";
      }} $out/share/kokoro-tts/voices-v1.0.bin

      cp ${pkgs.fetchurl {
        url = "https://github.com/nazdridoy/kokoro-tts/releases/download/v1.0.0/kokoro-v1.0.onnx";
        # nix-prefetch-url --type sha256 <url>
        sha256 = "1id66qvfzh2cfq44c8vpqcmvxvnh7w2qc9m32n08gcflyznghpbx";
      }} $out/share/kokoro-tts/kokoro-v1.0.onnx
    '';
  };

  # Wheel-only python packages installed with --no-deps.
  mkWheelPythonPackage = { pname, version, url, sha256 }:
    pkgs.stdenv.mkDerivation {
      inherit pname version;
      src = pkgs.fetchurl { inherit url sha256; };
      dontUnpack = true;
      nativeBuildInputs = [ pythonPackages.pip ];
      buildInputs = [ python ];
      dontBuild = true;

      installPhase = ''
        wheelBasename="$(basename "$src")"
        # Nix store paths prepend a hash prefix, which makes pip think the wheel filename is malformed.
        # Strip everything up to the first '-' so the basename becomes like '<dist>-<version>-...whl'.
        wheelFile="$(printf '%s' "$wheelBasename" | cut -d- -f2-)"

        tmpWheel="$TMPDIR/$wheelFile"
        cp "$src" "$tmpWheel"

        ${python}/bin/python -m pip install --no-deps --prefix=$out "$tmpWheel"
      '';
    };

  kokoroOnnx = mkWheelPythonPackage {
    pname = "kokoro-onnx";
    version = "0.3.9";
    url = "https://files.pythonhosted.org/packages/4e/1b/953b7f449a1a95f638a1108738144cf733e437fabfbdf2389c7452854363/kokoro_onnx-0.3.9-py3-none-any.whl";
    sha256 = "02g24rw1q0azws2bzqlabi5nq7zxjv3waqbw2nyymr1ba6rih61r";
  };

  espeakngLoader = mkWheelPythonPackage {
    pname = "espeakng-loader";
    version = "0.2.4";
    url = "https://files.pythonhosted.org/packages/de/1e/25ec5ab07528c0fbb215a61800a38eca05c8a99445515a02d7fa5debcb32/espeakng_loader-0.2.4-py3-none-manylinux_2_17_x86_64.manylinux2014_x86_64.whl";
    sha256 = "0vfbrlib55wqigyq9x2g4dl0vrvpaakdkvp6dcglcgfi4ypinwh8";
  };

  kokoroTts = pythonPackages.buildPythonApplication {
    pname = "kokoro-tts";
    version = "2.3.1";
    src = kokoroSrc;

    pyproject = true;
    nativeBuildInputs = [ pythonPackages.hatchling ];
    "build-system" = [ pythonPackages.hatchling ];
    doCheck = false;

    propagatedBuildInputs = [
      pythonPackages.beautifulsoup4
      pythonPackages.ebooklib

      kokoroOnnx
      espeakngLoader
      pythonPackages.phonemizer
      pythonPackages.colorlog
      pythonPackages.librosa
      pythonPackages.numba
      pythonPackages.onnxruntime

      pythonPackages.pymupdf
      pythonPackages.pymupdf4llm

      pythonPackages.sounddevice
      pythonPackages.soundfile
    ];

    # Upstream declares `pymupdf-layout` as a runtime dependency, but we rely on
    # nixpkgs' `pymupdf4llm` + `pymupdf` instead for this build.
    pythonRemoveDeps = [ "pymupdf-layout" ];

    pythonImportsCheck = [ "kokoro_tts" ];
  };

in
  if exposeModels then
    kokoroModels
  else
  pkgs.writeShellApplication {
    name = "kokoro-tts";
    runtimeInputs = [ kokoroTts ];
    text = ''
      set -euo pipefail

      MODEL_PATH="${kokoroModels}/share/kokoro-tts/kokoro-v1.0.onnx"
      VOICES_PATH="${kokoroModels}/share/kokoro-tts/voices-v1.0.bin"

      has_model=0
      has_voices=0
      for arg in "$@"; do
        if [ "$arg" = "--model" ]; then has_model=1; fi
        if [ "$arg" = "--voices" ]; then has_voices=1; fi
      done

      # Upstream parses input_text_file from sys.argv[1] and only scans options
      # later. So we must NOT prepend --model/--voices; append them instead.
      if [ "$has_model" -eq 0 ]; then
        set -- "$@" --model "$MODEL_PATH"
      fi
      if [ "$has_voices" -eq 0 ]; then
        set -- "$@" --voices "$VOICES_PATH"
      fi

      exec ${kokoroTts}/bin/kokoro-tts "$@"
    '';
  }
