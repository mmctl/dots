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

if command -v flatpak >/dev/null; then
  flatpak info org.signal.Signal >/dev/null 2>&1 && alias signal='flatpak run org.signal.Signal'
  flatpak info org.zotero.Zotero >/dev/null 2>&1 && alias zotero='flatpak run org.zotero.Zotero'
fi
