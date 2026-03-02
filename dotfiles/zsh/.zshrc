source ~/.zprofile

export GPG_TTY=$(tty)

# Enable colors and change prompt:
autoload -U colors && colors	# Load colors
setopt PROMPT_SUBST
# Detect OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    PS1='%{$fg[cyan]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%} >%{$reset_color%} '
else
    PS1='%{$fg[red]%}%n%{$fg[green]%} %{$fg[magenta]%}%~%{$fg[cyan]%} >%{$reset_color%} '
fi
setopt autocd		
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
source ~/.config/zsh/plugins/fzf-tab/fzf-tab.zsh

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

bindkey -s '^f' '^ucd "$(dirname "$(fzf)")"\n'

bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M vicmd '^[[P' vi-delete-char
bindkey -M vicmd '^e' edit-command-line
bindkey -M visual '^[[P' vi-delete

# syntax highlighting
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export PATH="/usr/local/bin:$PATH"

