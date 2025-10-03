with import <nixpkgs> { };

pkgs.writeShellScriptBin "play-yt-script" ''
  #!${pkgs.bash}

  ytformat=18
  while getopts ":f:" opt; do
    case $opt in
        f)
          echo "f optarg: $OPTARG"
          ytformat=$OPTARG
          ;;
        \?)
          echo "Invalid option: -$OPTARG" >&2
          exit 1
          ;;
        :)
          echo "Option -$OPTARG requires an argument." >&2
          exit 1
          ;;
    esac
  done
  shift $((OPTIND-1))

  url=$@
  regex="(twitch.tv|(youtube.com)/(watch|shorts))"

  if [[ "$url" =~ $regex ]]; then
    echo "found regex in input arg"
  else
    url=$(xclip -o)
    [[ "$url" =~ $regex ]] && echo "found regex in clipboard" || url=$(${pkgs.rofi}/bin/rofi -dmenu -p "Enter video url")
  fi

  if [[ -n $url ]]; then
     notify-send -t 3000 "Starting mpv: $url"
     if [[ "$url" =~ (youtube\.com|youtu\.be) ]]; then
        # yt-dlp -f 18 -o - "$url" | vlc -
        mpv --ytdl-format=$ytformat "$url"
     else
        mpv "$url"
     fi

     if [ $? -ne 0 ]; then
       notify-send -t 3000 "ERROR: Unable to start mpv with url: $url"
     fi
  fi
''
