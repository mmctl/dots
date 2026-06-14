# 30-zoxide.zsh

# Zoxide integration, replacing cd
if (( $+commands[zoxide] )); then
    eval "$(command zoxide init zsh --cmd cd)"
fi
