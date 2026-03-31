# Bash completions
[[ -f /etc/profile.d/bash_completion.sh ]] && . /etc/profile.d/bash_completion.sh


# FZF
if command -v fzf > /dev/null; then
    ble-import -d integration/fzf-completion
    ble-import -d integration/fzf-key-bindings
    ble-import -d integration/fzf-menu
fi


# Zoxide
if command -v zoxide > /dev/null; then
   eval "$(zoxide init bash --cmd cd)"
   ble-import -d integration/zoxide
fi


# Markers
bleopt prompt_eol_mark=
bleopt exec_errexit_mark=
bleopt exec_elapsed_mark=
bleopt exec_exit_mark=
bleopt edit_marker=
bleopt edit_marker_error=


# Color scheme and faces
ble-import contrib/colorglass
[[ -f "$XDG_CONFIG_HOME/blesh/colors.sh" ]] && . "$XDG_CONFIG_HOME/blesh/colors.sh"


# Bindings
ble-bind -m emacs -f 'C-/' 'emacs/undo'
ble-bind -m emacs -f 'M-/' 'emacs/redo'
ble-bind -m emacs -f 'C-x u' 'emacs/undo'
ble-bind -m emacs -f 'C-x C-u' 'emacs/undo'
ble-bind -m emacs -f 'C-x r' 'emacs/redo'
ble-bind -m emacs -f 'C-x C-r' 'emacs/redo'

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
