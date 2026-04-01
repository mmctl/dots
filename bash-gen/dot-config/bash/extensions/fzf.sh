# FZF
if command -v fzf >/dev/null && command -v fd >/dev/null; then
    _fzf_compgen_path() {
        fd --hidden --color=always --exclude ".git" . "$1"
    }

    _fzf_compgen_dir() {
        fd --type d --hidden --color=always --exclude ".git" . "$1"
    }

    export FZF_ALT_C_OPTS='--walker=dir,hidden'
fi
