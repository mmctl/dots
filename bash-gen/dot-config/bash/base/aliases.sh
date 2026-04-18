[[ $- != *i* ]] && return

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias md='mkdir -pv'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

if command -v emacsclient >/dev/null; then
  alias ecc='emacsclient -c --alternate-editor='
fi
