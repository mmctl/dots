# Overrides
## FZF (completion)
_fzf_compgen_path() {
  fd --hidden --color=always . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --type e --hidden --color=always . "$1"
}
