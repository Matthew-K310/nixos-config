#!/usr/bin/env bash
HOSTNAME=$(hostname)

if [ "$HOSTNAME" = "initium" ]; then
	waybar --config ~/.config/waybar/laptop-config.jsonc
else
	waybar --config ~/.config/waybar/desktop-config.jsonc
fi
