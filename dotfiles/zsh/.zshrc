source ~/.zprofile

export GPG_TTY=$(tty)

if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
setopt PROMPT_SUBST
# parse_git_branch() {
#   git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/[\1]/p'
# }
# PS1='%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%}$(parse_git_branch)%{$fg[red]%}]%{$reset_color%} '
# PS1='$fg[yellow]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%}$(parse_git_branch)%{$fg[red]%} >%{$reset_color%} '
# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    PS1='%{$fg[cyan]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%} >%{$reset_color%} '
else
    PS1='%{$fg[red]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%} >%{$reset_color%} '
fi
setopt autocd		# Automatically cd into typed directory.
setopt interactive_comments

# History
HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE="$XDG_CACHE_HOME/zsh_history" # move histfile to cache
HISTCONTROL=ignoreboth # consecutive duplicates & commands starting with space are not saved
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Load aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"

# Autocomplete
zmodload zsh/complist
autoload -U compinit && compinit
# cmp opts
zstyle ':completion:*' menu select # tab opens cmp menu
zstyle ':completion:*' special-dirs true # force . and .. to show in cmp menu
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} ma=0\;33 # colorize cmp menu
zstyle ':completion:*' squeeze-slashes false # explicit disable to allow /*/ expansion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# better tab completion
source /usr/local/share/zsh/plugins/fzf-tab/fzf-tab.zsh

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# cursor blink in insert mode only
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[0 q';;      # block
        viins|main) echo -ne '\e[1 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[1 q"
}
zle -N zle-line-init
echo -ne '\e[1 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[1 q' ;} # Use beam shape cursor for each new prompt.

# load zoxide
eval "$(zoxide init zsh)"

# fzf setup
source <(fzf --zsh) # allow for fzf history widget

# Use lf to switch directories and bind it to ctrl-o
# lfcd () {
#     tmp="$(mktemp -uq)"
#     trap 'rm -f $tmp >/dev/null 2>&1 && trap - HUP INT QUIT TERM EXIT' HUP INT QUIT TERM EXIT
#     lf -last-dir-path="$tmp" "$@"
#     if [ -f "$tmp" ]; then
#         dir="$(cat "$tmp")"
#         [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
#     fi
# }

# Use yazi to switch directories and bind it to ctrl-o
# function ycd() {
# 	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
# 	yazi "$@" --cwd-file="$tmp"
# 	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
# 		builtin cd -- "$cwd"
# 	fi
# 	rm -f -- "$tmp"
# }

# bindkey -s '^o' '^uycd\n'

# bindkey -s '^a' '^ubc -lq\n'

bindkey -s '^f' '^ucd "$(dirname "$(fzf)")"\n'

bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M vicmd '^[[P' vi-delete-char
bindkey -M vicmd '^e' edit-command-line
bindkey -M visual '^[[P' vi-delete

# # autosuggestions
# # source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
#
# # Shell integrations
# eval "$(pyenv init --path)"
# # eval "$(oh-my-posh init zsh --config $HOME/.config/ohmyposh/ompconfig.toml)"

# tmux session startup
# if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
#   tmux attach-session -t default || tmux new-session -s default
# fi

# # Open yazi with ctrl-n keybind
# bindkey -s '^o' 'y\n'
#
# # move to current directory on Yazi close
#
# syntax highlighting
source /usr/local/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export PATH="/usr/local/bin:$PATH"

# bun completions
[ -s "/home/matthewkennedy/.bun/_bun" ] && source "/home/matthewkennedy/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

## [Completion]
## Completion scripts setup. Remove the following line to uninstall
[[ -f /home/matthewkennedy/.config/.dart-cli-completion/zsh-config.zsh ]] && . /home/matthewkennedy/.config/.dart-cli-completion/zsh-config.zsh || true
## [/Completion]


[ -f "/home/matthewkennedy/.ghcup/env" ] && . "/home/matthewkennedy/.ghcup/env" # ghcup-env
