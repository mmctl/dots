# 99-plugins.zsh

() {
    local plugdir="${ZPLUGDIR:-${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins}"

    if (( $+commands[fzf] )) &&
           [[ -r "$plugdir/fzf-tab/fzf-tab.plugin.zsh" ]]; then
        source "$plugdir/fzf-tab/fzf-tab.plugin.zsh"
    fi

    [[ -r "$plugdir/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] &&
        source "$plugdir/zsh-autosuggestions/zsh-autosuggestions.zsh"

    [[ -r "$plugdir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] &&
        source "$plugdir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
}
