with import <nixpkgs> { };

pkgs.writeShellScriptBin "my-tts" ''
  #!${pkgs.bash}
  echo $@ | piper --model ~/piper/models/en_US-lessac-high.onnx --output-raw | aplay -r 22050 -f S16_LE -t raw -
''