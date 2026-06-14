# aliases.zsh

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias md='mkdir -pv'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

if (( $+commands[emacsclient] )); then
    alias ecc='emacsclient -c --alternate-editor='
fi

if (( $+commands[flatpak] )); then
    alias signal='flatpak run org.signal.Signal'
    alias zotero='flatpak run org.zotero.Zotero'
    alias zen='flatpak run app.zen_browser.zen -P "$USER"'
fi
