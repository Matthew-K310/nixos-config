#!/usr/bin/env bash
exec 2>"$XDG_RUNTIME_DIR/waybar-playerctl.log"
IFS=$'\n\t'

escape() {
    local s="$1"
    s="${s//\"/\\\"}"
    printf '%s' "$s"
}

escape_markup() {
    local s="$1"
    s="${s//&/&amp;}"
    s="${s//</&lt;}"
    s="${s//>/&gt;}"
    printf '%s' "$s"
}

json_out() {
    printf '{"text":"%s","tooltip":"%s","class":"%s"}\n' \
        "$(escape "$1")" "$(escape "$2")" "$3"
}

# Pick the highest-priority active player
choose_best_player() {
    # 1: players currently playing
    if playerctl metadata --format '{{playerName}} {{status}}' 2>/dev/null \
        | grep "Playing" >/dev/null; then
        playerctl metadata --format '{{playerName}} {{status}}' \
        | awk '/Playing/ {print $1; exit}'
        return
    fi

    # 2: paused players
    if playerctl metadata --format '{{playerName}} {{status}}' 2>/dev/null \
        | grep "Paused" >/dev/null; then
        playerctl metadata --format '{{playerName}} {{status}}' \
        | awk '/Paused/ {print $1; exit}'
        return
    fi

    # 3: fallback → first available MPRIS player
    playerctl -l 2>/dev/null | head -n1
}

# Stream all metadata updates from *all* players
playerctl --all-players --follow metadata --format \
    $'{{playerName}}\t{{status}}\t{{position}}\t{{mpris:length}}\t{{artist}} - {{title}}' \
| while read -r player status pos length line; do

    best=$(choose_best_player)

    # ignore events from non-best players
    [ "$player" != "$best" ] && continue

    if [ -z "$line" ]; then
        json_out '<span foreground="#dc322f">⏹</span>' "Stopped" "0"
        continue
    fi

    esc_line=$(escape_markup "$line")

    # compute progress %
    if [ -n "$length" ] && [ "$length" -gt 0 ] 2>/dev/null; then
        percentage=$(( 100 * (pos % length) / length ))
    else
        percentage=0
    fi

    case "$status" in
        Paused)
            text='<span foreground="#b4bec8" size="smaller">'"$esc_line"'</span>'
            ;;
        Playing)
            text="<small>$esc_line</small>"
            ;;
        *)
            text='<span foreground="#e0dcd4">⏹</span>'
            ;;
    esac

    json_out "$text" "$status" "$percentage"
done
