# 40-bindings.zsh

bindkey -e

# Region handling
bindkey '^@'   set-mark-command
bindkey '^X^X' exchange-point-and-mark
bindkey '^[w'  copy-region-as-kill
bindkey '^W'   kill-region

# Word deletion
bindkey '^H'      backward-kill-word
bindkey '^[[3;5~' kill-word

# Undo and redo
bindkey '^_'  undo
bindkey '^[/' redo
