# FZF
if command -v fzf >/dev/null; then
    export FZF_ALT_C_OPTS='--walker=dir,hidden'

    if  command -v fd >/dev/null; then
    _fzf_compgen_path() {
        fd --hidden --color=always --exclude ".git" . "$1"
    }

    _fzf_compgen_dir() {
        fd --type d --hidden --color=always --exclude ".git" . "$1"
    }
    fi
fi
