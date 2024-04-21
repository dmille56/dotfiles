with import <nixpkgs> { };

pkgs.writeShellScriptBin "my-tts" ''
  #!${pkgs.bash}
  if read -t 0; then
      # handle input from stdin
      input=$(cat)
      echo "$input" | piper --model ~/piper/models/en_US-lessac-high.onnx --output-raw | aplay -r 22050 -f S16_LE -t raw -
  else
      # handle input from an argument
      echo $@ | piper --model ~/piper/models/en_US-lessac-high.onnx --output-raw | aplay -r 22050 -f S16_LE -t raw -
  fi
''
