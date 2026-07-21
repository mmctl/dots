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
  # alias ecc='emacsclient -c -n --alternate-editor='
  alias ecc='emacsclient-x11 -c -n'
fi

if command -v flatpak >/dev/null; then
  alias signal='flatpak run org.signal.Signal'
  alias zotero='flatpak run org.zotero.Zotero'
  alias zen="flatpak run app.zen_browser.zen -P $USER"
fi
