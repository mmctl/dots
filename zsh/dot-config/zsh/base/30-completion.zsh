# 30-completion.zsh

autoload -Uz compinit

local completion_cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
mkdir -p -- "$completion_cache"

compinit -d "$completion_cache/zcompdump"

# Interactive menu selection requires normal terminal cursor capabilities
if [[ ${TERM:-dumb} != dumb ]]; then
    zmodload zsh/complist
    zstyle ':completion:*' menu select
fi
