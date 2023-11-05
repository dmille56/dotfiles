with import <nixpkgs> { };

pkgs.writeShellScriptBin "play-yt-script-format" ''
  #!${pkgs.bash}
  # TODO: finish this

  url=$(${pkgs.rofi}/bin/rofi -dmenu -p "Youtube url")
  if [[ -n $url ]]; then
     formatList=$(yt-dlp -F "$url" | sed '1,/-----------------------/d' | sort -r -n -k 3)
     if [[ -n $formatList ]]; then
        selectedFormat=$(echo "$formatList" | ${pkgs.rofi}/bin/rofi -dmenu -p "Youtube format" | awk '{print $1}')
        if [[ -n $selectedFormat ]]; then
           # yt-dlp -f $selectedFormat -o - "$url" | vlc -
           mpv --ytdl-format=$selectedFormat "$url"
        else
           notify-send "Unable to get selected format"
        fi
     else
        notify-send "Unable to get format list from yt-dlp"
     fi
  else
    notify-send "Unable to get url from rofi"
  fi
''
