# 99-plugins.zsh

() {
    local dir="${ZPLUGDIR:-${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins}"

    [[ -r "$dir/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] &&
        source "$dir/zsh-autosuggestions/zsh-autosuggestions.zsh"

    [[ -r "$dir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] &&
        source "$dir/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
}
