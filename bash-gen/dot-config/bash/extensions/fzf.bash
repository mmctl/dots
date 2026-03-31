# FZF
if command -v fzf >/dev/null && command -v fd >/dev/null; then
    _fzf_compgen_path() {
        fd --hidden --color=always . "$1"
    }

    _fzf_compgen_dir() {
        fd --type d --type e --hidden --color=always . "$1"
    }
fi
