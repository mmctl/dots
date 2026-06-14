# 30-completion.zsh

zstyle ':completion:*' menu no

if [[ -n ${LS_COLORS-} ]]; then
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

() {
    local cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"

    mkdir -p -- "$cache_dir"

    autoload -Uz compinit
    compinit -d "$cache_dir/zcompdump"
}
