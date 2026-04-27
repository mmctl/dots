# Bash completions
BASH_COMPLETION_COMPAT_IGNORE=fzf
[[ -f /etc/profile.d/bash_completion.sh ]] && . /etc/profile.d/bash_completion.sh


# FD
command -v fd >/dev/null && _cached_eval fd-completions fd --gen-completions bash


# Ripgrep/RG
command -v rg >/dev/null && _cached_eval ripgrep-completions rg --generate complete-bash


# FZF
if command -v fzf >/dev/null; then
    ble-import -d integration/fzf-completion
    ble-import -d integration/fzf-key-bindings
    ble-import -d integration/fzf-menu
fi


# Zoxide
if command -v zoxide >/dev/null; then
    _cached_eval zoxide-init-cd zoxide init bash --cmd cd
    ble-import -d integration/zoxide
fi


# Markers
bleopt prompt_eol_mark=
bleopt exec_errexit_mark=
bleopt exec_elapsed_mark=
bleopt exec_exit_mark=
bleopt edit_marker=
bleopt edit_marker_error=

# History
bleopt history_share=1
bleopt history_erasedups_limit=2500

# Autocomplete
bleopt complete_ambiguous=

# Highlight
bleopt highlight_filename=

# Color scheme and faces
[[ -f "$XDG_CONFIG_HOME/blesh/colors.sh" ]] && . "$XDG_CONFIG_HOME/blesh/colors.sh"


# Bindings
ble-bind -m emacs -f 'C-h' 'delete-backward-cword'
ble-bind -m emacs -f 'C-DEL' 'delete-backward-cword'
ble-bind -m emacs -f 'C-BS' 'delete-backward-cword'

ble-bind -m emacs -f 'C-/' 'emacs/undo'
ble-bind -m emacs -f 'M-/' 'emacs/redo'


# Custom
# User-specific runcommands (extensions)
config_extensions_dir="$XDG_CONFIG_HOME/blesh/extensions"
if [[ -d "$config_extensions_dir" ]]; then
    shopt -s nullglob
    for rc in "$config_extensions_dir"/*; do
        [[ -f "$rc" ]] && ble-import -d "$rc"
    done
    shopt -u nullglob
fi
unset rc
