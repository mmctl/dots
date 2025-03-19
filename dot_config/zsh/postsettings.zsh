# General
zstyle ':completions:*' menu no
zstyle ':completions:*' list-colors ${(s.:.)LS_COLORS}


# Plugins
## FZF (-tab)
zstyle ':fzf-tab:*' query-string prefix input first
zstyle ':fzf-tab:*' fzf-flags --height=~60% --tiebreak=begin,chunk,length,index
zstyle ':fzf-tab:*' fzf-pad 0
zstyle ':fzf-tab:*' fzf-min-height 16


## Autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=100

## Syntax highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets root)
ZSH_HIGHLIGHT_MAXLENGTH=256

## You should use
export YSU_MESSAGE_POSITION="after"
export YSU_MESSAGE_FORMAT="$(tput bold)$(tput setaf 3)Found existing alias for \"$(tput setaf 5)%command$(tput setaf 3)\". You should use \"$(tput setaf 5)%alias$(tput setaf 3)\".$(tput sgr0)"
