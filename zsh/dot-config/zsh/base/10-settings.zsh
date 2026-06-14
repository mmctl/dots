# settings.zsh

# History
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
HISTSIZE=10000
SAVEHIST=10000

mkdir -p -- "${HISTFILE:h}"

setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY_TIME

setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# Interactive behavior
setopt AUTO_CD
setopt INTERACTIVE_COMMENTS
