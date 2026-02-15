#!/usr/bin/env bash
exec 2>"$XDG_RUNTIME_DIR/waybar-playerctl.log"
IFS=$'\n\t'

while true; do

	while read -r playing position length line; do
		# json escaping
		line="${line//\"/\\\"}"
		((percentage = length ? (100 * (position % length)) / length : 0))
		case $playing in
		    Paused) text='<span foreground=\"#b4bec8\" size=\"smaller\">'"$line"'</span>' ;;
		    Playing) text="<small>$line</small>" ;;
		    *) text='<span foreground=\"#e0dcd4\">⏹</span>' ;;
		esac

		# exit if print fails
		printf '{"text":"%s","tooltip":"%s","class":"%s"}\n' \
			"$text" "$playing" || break 2

    done < <(
        # Determine which player should take priority
        PLAYER_ARGS="cider,mpd,mpv,%any"

        # If cider exists but is NOT playing → let MPD take priority
        if playerctl --player=cider status 2>/dev/null | grep -qv Playing; then
            PLAYER_ARGS="mpd,cider,mpv,%any"
        fi

        # requires playerctl>=2.0
        playerctl --follow metadata --player "$PLAYER_ARGS" --format \
            $'{{status}}\t{{position}}\t{{mpris:length}}\t{{markup_escape(artist)}} - {{markup_escape(title)}}' &
        echo $! >"$XDG_RUNTIME_DIR/waybar-playerctl.pid"
    )

	# done < <(
	# 	# requires playerctl>=2.0
	# 	playerctl --follow metadata --player mpd,cider,%any --format \
	    # 		$'{{status}}\t{{position}}\t{{mpris:length}}\t{{markup_escape(artist)}} - {{markup_escape(title)}}' &
	# 	echo $! >"$XDG_RUNTIME_DIR/waybar-playerctl.pid"

	# )

	# no current players
	# exit if print fails
	echo '<span foreground=#dc322f>⏹</span>' || break
	sleep 15

done

kill "$(<"$XDG_RUNTIME_DIR/waybar-playerctl.pid")"
