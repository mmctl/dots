export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"

eval "$(starship init zsh)"

export STARSHIP_CACHE="$XDG_CACHE_HOME/starship/session-${STARSHIP_SESSION_KEY}.log"
