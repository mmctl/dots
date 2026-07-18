# 40-bindings.zsh

WORDCHARS=${WORDCHARS//\//} # Exclude path separators from word characters

bindkey -e

# Word movement
bindkey '^P' backward-word  # Ctrl-p
bindkey '^N' forward-word  # Ctrl-n
bindkey '^[[1;5D' backward-word  # Ctrl-Left
bindkey '^[[1;5C' forward-word   # Ctrl-Right

# Mark and region handling.
bindkey '^@'   set-mark-command
bindkey '^X^X' exchange-point-and-mark
bindkey '^[w'  copy-region-as-kill
bindkey '^W'   kill-region

# Character deletion, respecting an active region.
backward-delete-char-or-region() {
    if (( REGION_ACTIVE )); then
        zle kill-region
    else
        zle backward-delete-char
    fi
}
zle -N backward-delete-char-or-region

delete-char-or-region() {
    if (( REGION_ACTIVE )); then
        zle kill-region
    else
        zle delete-char
    fi
}
zle -N delete-char-or-region

bindkey '^?'    backward-delete-char-or-region
bindkey '^[[3~' delete-char-or-region

# Word deletion, also respecting an active region.
backward-kill-word-or-region() {
    if (( REGION_ACTIVE )); then
        zle kill-region
    else
        zle backward-kill-word
    fi
}
zle -N backward-kill-word-or-region

kill-word-or-region() {
    if (( REGION_ACTIVE )); then
        zle kill-region
    else
        zle kill-word
    fi
}
zle -N kill-word-or-region

bindkey '^H'         backward-kill-word-or-region
bindkey '^[[127;5u'  backward-kill-word-or-region
bindkey '^[[3;5~'    kill-word-or-region

# Undo and redo
bindkey '^_' undo
bindkey '^[_' redo

# Completion/expansion
bindkey '^I' complete-word
