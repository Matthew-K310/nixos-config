#!/usr/bin/env bash

# Query MPD for current track, handle disconnection gracefully
# if ! /run/current-system/sw/bin/mpc current -f '%artist% - %title%' 2>/dev/null; then
#     echo ""
# fi

PLAYER_ARGS="cider,mpd,mpv,%any"

# If cider exists but is NOT playing → let MPD take priority
if playerctl --player=cider status 2>/dev/null | grep -qv Playing; then
    PLAYER_ARGS="kdeconnect,mpc,cider,mpv,%any"
fi

# requires playerctl>=2.0
if playerctl --follow metadata --player "$PLAYER_ARGS" --format $'{{markup_escape(artist)}} - {{markup_escape(title)}}' 2>/dev/null; then
    echo""
fi
