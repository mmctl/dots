bindkey '^[OB' down-line-or-beginning-search # Down arrow
bindkey '^[OA' up-line-or-beginning-search # Up arrow
bindkey "^[OD" backward-char # Left arrow
bindkey "^[OC" forward-char # Right arrow
bindkey '^[[1;5D' backward-word # Control + Left arrow
bindkey '^[[1;5C' forward-word # Control + Right arrow
bindkey '^[[1;5H' beginning-of-buffer-or-history # Control + Home
bindkey '^[[1;5F' end-of-buffer-or-history # Control + End
bindkey '^B' fzf-cd-widget
bindkey '^F' fzf-file-widget
bindkey '^H' fzf-history-widget
bindkey '^L' fzf-completion
bindkey '^T' copy-region-as-kill
bindkey '^O' autosuggest-accept
bindkey '^P' autosuggest-execute
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^[^?' backward-kill-word # Alt + Backspace
bindkey '^[c' clear-screen
bindkey '^[a' backward-kill-line
bindkey '^[e' kill-line
bindkey '^[l' kill-whole-line
bindkey '^[w' kill-word
bindkey '^[t' kill-region
bindkey '^[y' yank
bindkey '^[;' undo
bindkey '^[/' redo
