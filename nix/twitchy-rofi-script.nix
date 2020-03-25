with import <nixpkgs> {};

pkgs.writeShellScriptBin "twitchy-rofi-script" ''
  #!${pkgs.bash}

  # Recommend to set twitchy non-interactive options (in ~/.config/twitchy3/twitchy.cfg) to:
  # DisplayScheme = ChannelName, GameName, Status, Viewers
  # Delimiter = ,
  
  quality='360p'
  
  selected=$(twitchy --non-interactive | sort | column -s\, -t | rofi -dmenu -i -p "Select Twitch stream")
  target=$(echo "$selected" | cut -f1 -d" ")
  
  if [[ -n $target ]]; then
      streamlink --player="mpv --fs" twitch.tv/$target $quality
  fi
''
