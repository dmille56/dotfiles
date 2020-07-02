with import <nixpkgs> { };

pkgs.writeShellScriptBin "twitchy-emacs-play-script" ''
  #!${pkgs.bash}
  ${pkgs.xdotool}/bin/xdotool search --desktop 0 Emacs windowactivate --sync key --delay 24 alt+x type --delay 24 twitchy-play-stream
  sleep 0.1 
  ${pkgs.xdotool}/bin/xdotool search --desktop 0 Emacs windowactivate --sync key KP_Enter
''
