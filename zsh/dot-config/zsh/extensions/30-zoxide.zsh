# 30-zoxide.zsh

if (( $+commands[zoxide] )); then
    eval "$(command zoxide init zsh --cmd cd)"
fi
