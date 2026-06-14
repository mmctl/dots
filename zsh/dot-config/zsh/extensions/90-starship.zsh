# 90-starship.zsh

if (( $+commands[starship] )); then
    eval "$(command starship init zsh)"
fi
