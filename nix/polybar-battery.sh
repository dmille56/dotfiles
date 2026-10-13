#!/usr/bin/env bash
set -eu

# Read the system battery without depending on a particular BAT or AC name.
for battery_path in /sys/class/power_supply/*; do
  [[ -r "$battery_path/type" && -r "$battery_path/capacity" && -r "$battery_path/status" ]] || continue
  [[ $(<"$battery_path/type") == Battery ]] || continue
  if [[ -r "$battery_path/scope" && $(<"$battery_path/scope") == Device ]]; then
    continue
  fi

  read -r capacity < "$battery_path/capacity"
  read -r status < "$battery_path/status"
  [[ "$capacity" =~ ^[0-9]+$ ]] || continue

  color='#8BE9FD'
  icon=''
  case "$status" in
    Charging) color='#F1FA8C'; icon=' ' ;;
    Full) color='#50FA7B' ;;
    *) if (( capacity <= 10 )); then color='#FF5555'; fi ;;
  esac
  printf '%%{F%s}%s %3s%%%%{F-}\n' "$color" "$icon" "$capacity"
  exit 0
done
