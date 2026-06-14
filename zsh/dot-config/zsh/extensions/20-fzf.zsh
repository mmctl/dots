# fzf.zsh

if (( $+commands[fzf] )); then
    source <(command fzf --zsh)

    FZF_ALT_C_OPTS='--walker=dir,hidden'

    if (( $+commands[fd] )); then
        _fzf_compgen_path() {
            command fd --hidden --exclude .git . "$1"
        }

        _fzf_compgen_dir() {
            command fd --type d --hidden --exclude .git . "$1"
        }
    fi
fi
