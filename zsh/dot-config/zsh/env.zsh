# Directories
## ZSH (OMZ)
ZSH="$XDG_CONFIG_HOME/zsh/oh-my-zsh" ## Oh my Zsh installation
ZSH_CUSTOM="$XDG_DATA_HOME/oh-my-zsh" ## Additional custom folder (on top>
ZSH_CACHE_DIR="$XDG_CACHE_HOME/oh-my-zsh"

## Yazi
export YAZI_CONFIG_HOME="$XDG_CONFIG_HOME/yazi"


# Files
ZSH_COMPDUMP="$ZSH_CACHE_DIR/.zcompdump"
HISTFILE="$ZSH_CACHE_DIR/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000

# Behavior
## LS
[[ -f "$XDG_CONFIG_HOME/dircolors/.dir_colors" && -r "$XDG_CONFIG_HOME/dircolors/.dir_colors" ]] && eval $(dircolors "$XDG_CONFIG_HOME/dircolors/.dir_colors")

## FZF (shell integration)
export FZF_DEFAULT_COMMAND='fd --hidden --color=always'
export FZF_COMPLETION_TRIGGER='%%'
export FZF_COMPLETION_PATH_OPTS='--tiebreak=begin,chunk,length,index --walker file,dir,hidden'
export FZF_COMPLETION_DIR_OPTS='--tiebreak=begin,chunk,length,index --walker dir,hidden'
export FZF_CTRL_T_OPTS='--walker=file,hidden'
export FZF_ALT_C_OPTS='--walker=dir,hidden --walker-root=/'

