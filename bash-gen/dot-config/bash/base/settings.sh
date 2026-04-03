[[ $- != *i* ]] && return

HISTSIZE=2000
HISTFILESIZE=5000
HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='ls:bg:fg:history:clear:exit'
HISTTIMEFORMAT='%F %T '

shopt -s histappend cmdhist lithist
shopt -s autocd cdspell dirspell no_empty_cmd_completion
shopt -s checkwinsize

IGNOREEOF=3
