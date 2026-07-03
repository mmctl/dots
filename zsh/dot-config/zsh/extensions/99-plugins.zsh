# 99-plugins.zsh

() {
    local plugdir="${ZPLUGDIR:-${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins}"

    if (( $+commands[fzf] )) &&
           [[ -r "$plugdir/fzf-tab/fzf-tab.plugin.zsh" ]]; then
        source "$plugdir/fzf-tab/fzf-tab.plugin.zsh"
        zstyle ':fzf-tab:*' fzf-flags \
               --height='~60%' \
               --wrap \
               --scroll-off=3 \
               --hscroll-off=10 \
               --layout=reverse \
               --border=rounded \
               --margin=0,2.5% \
               --padding=0 \
               --info=inline-right \
               --ansi \
               --highlight-line
    fi

    [[ -r "$plugdir/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] &&
        source "$plugdir/zsh-autosuggestions/zsh-autosuggestions.zsh"

    [[ -r "$plugdir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] &&
        source "$plugdir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
}
