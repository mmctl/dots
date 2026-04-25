[[ $- != *i* ]] && return

# History
mkdir -p "${XDG_STATE_HOME:-$HOME/.local/state}/bash"

HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/bash/history"
HISTSIZE=2500
HISTFILESIZE=5000
HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='ls:bg:fg:history:clear:exit'
HISTTIMEFORMAT='%F %T '

shopt -s histappend cmdhist lithist
shopt -s autocd cdspell dirspell no_empty_cmd_completion
shopt -s checkwinsize

IGNOREEOF=3
