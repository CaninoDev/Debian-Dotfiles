
# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$HOME/.npm-global/bin:$PATH

# Path to your oh-my-zutosh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="norm"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# For oh-my-zsh Tmux plugin to start:
export ZSH_TMUX_AUTOSTART=false

plugins=(battery git history colored-man ssh-agent colorize sublime history-substring-search compleat ubuntu yarn nice-exit-code elixir)

source $HOME/.oh-my-zsh/oh-my-zsh.sh

# Setup syntax highlighing
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGLIGHTERS=( main brackets cursor line root pattern )
ZSH_HIGHLIGHT_STYLES[cursor]='bg=red'

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/id_rsa"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
export NODE_PATH=/usr/lib/nodejs:/usr/lib/node_modules:/usr/share/javascript:/home/ianga/.npm-global/lib/node_modules:/home/ianga/.npm-global/lib/node_modules

# Set $TERM
case "$TERM" in
    xterm*)
	    if [ -e /lib/terminfo/x/xterm-256color ]; then
		    export TERM=xterm-256color
	    elif [ -e /lib/terminfo/x/xterm-color ]; then
		    export TERM=xterm-color;
	    else
		    export TERM=xterm
	    fi
	    ;;
    screen*)
	    if [ -e /lib/terminfo/s/screen-256color ]; then
		    export TERM=screen-256color;
	    elif [ -e /lib/terminfo/s/screen-256color-bce-s ]; then
		    export TERM=screen-256color-bce-s ];
	    else
		    export TERM=screen
	    fi
	    ;;
    linux*)
      if [ -n "$FBTERM" ]; then
        export TERM=fbterm
				# Source the color calibration for fbterm
				source /usr/local/sbin/fbinit.zsh
      fi
      ;;
esac

#Set PATHS
export PATH="$PATH:$HOME/.rvm/bin:$HOME/.npm-global/bin" # Add RVM to PATH for scripting
export PATH="$PATH:$HOME/.npm-global/bin" # Add local NPM global directory for execution
#
# Alias for primary server
alias oldboy='ssh -XY ianga@oldboy.bittencock.com'
# Run fortune
/usr/games/fortune
echo "\n"
# alias config for dotfiles
alias config='/usr/bin/git --git-dir=/home/caninodev/.dotfiles/'
alias pi='ssh -XY pilittle.bittencock.com'

export GOROOT=/home/caninodev/.local/share/go
#export PATH=/home/caninodev/bin:/usr/local/bin:/home/caninodev/.npm-global/bin:/home/caninodev/bin:/usr/local/bin:/home/caninodev/.npm-global/bin:/home/caninodev/bin:/home/caninodev/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/home/caninodev/.rvm/bin:/home/caninodev/.npm-global/bin:/home/caninodev/.npm-global/bin:/home/caninodev/.rvm/bin:/home/caninodev/.npm-global/bin:/home/caninodev/.npm-global/bin:/home/caninodev/.local/share/go/bin
