#!/usr/bin/env zsh

#!/usr/bin/env bash
# Dependencies: grim, slurp, wl-copy, notify-send, jq, swaymsg or hyprctl

# Directory to save screenshots
SAVEDIR="$HOME/cloud/photos/screenshots"
mkdir -p "$SAVEDIR"

# Timestamped filename
FILE="$SAVEDIR/$(date +'%Y%m%d-%H%M%S.png')"

# Choose mode
MODE="$1"

case "$MODE" in
  zone)
    # Select region with slurp
    grim -g "$(slurp)" - | tee "$FILE" | wl-copy
    notify-send "Zone screenshot captured" "Copied to clipboard"
    ;;
  
  full)
    # Capture full output
    grim - | tee "$FILE" | wl-copy
    notify-send "Full screenshot captured" "Copied to clipboard"
    ;;
  
  *)
    echo "Usage: $0 {zone|window|full}"
    exit 1
    ;;
esac
