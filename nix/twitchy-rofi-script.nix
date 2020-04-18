with import <nixpkgs> { };

pkgs.writeShellScriptBin "twitchy-rofi-script" ''
  #!${pkgs.bash}

  # Set twitchy non-interactive options (in ~/.config/twitchy3/twitchy.cfg) to:
  # DisplayScheme = ChannelName, GameName, Viewers, Status
  # Delimiter = ,

  quality='360p'

  selected=$(twitchy --non-interactive | tr -cd "[:print:]\n" | sort -t, -k2,2 -k3,3nr | column -s\, -t | rofi -dmenu -i -p "Select Twitch stream")
  target=$(echo "$selected" | cut -f1 -d" ")

  if [[ -n $target ]]; then
      streamlink --player="mpv --fs" twitch.tv/$target $quality
  fi
''
