# Theme
ZSH_THEME=""

# Add completion functions
fpath+="$ZSH_CUSTOM/plugins/zsh-completions/src"

# Source FZF zsh scripts (widgets for history, file, 
# and completion search)
. <(fzf --zsh)

# Plugins
plugins=(
    git
    fzf-tab
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-you-should-use
    autoupdate
)

# Updates zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 7
zstyle ':omz:update' verbosity minimal

# Completion
COMPLETION_WAITING_DOTS=true
CASE_SENSITIVE=false
HYPHEN_INSENSITIVE=true

# Terminal title
DISABLE_AUTO_TITLE=false

# Library
DISABLE_MAGIC_FUNCTIONS=true
DISABLE_LS_COLORS=true
ENABLE_CORRECTION=true
DISABLE_UNTRACKED_FILES_DIRTY=true
HIST_STAMPS="%Y/%m/%d"
