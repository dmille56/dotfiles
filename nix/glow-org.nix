{ pkgs }:

pkgs.writeShellScriptBin "glow-org" ''
  #!${pkgs.bash}

  if [ $# -eq 0 ]; then
    echo "Usage: glow-org <file.org> [options]" >&2
    exit 1
  fi
  ${pkgs.pandoc}/bin/pandoc -t markdown "$1" | ${pkgs.glow}/bin/glow
''
