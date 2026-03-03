#!/usr/bin/env bash

# Emoji picker using wofi on Wayland (Arch version)
emoji_file="$HOME/.local/share/chars/"*

# Pre-process the file to avoid wofi recalculating size
chosen=$(cut -d ';' -f1 $emoji_file | wofi --dmenu \
	--prompt "Pick emoji:" \
	--lines 10 \
	--cache-file /dev/null \
	--insensitive |
	sed "s/ .*//")

[ -z "$chosen" ] && exit

if [ -n "$1" ]; then
	wtype "$chosen" # type directly
else
	printf "%s" "$chosen" | wl-copy
	notify-send "Emoji copied" "'$chosen' copied to clipboard."
fi
