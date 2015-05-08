# ~/.zshrc

# General options
export XAUTHORITY=$HOME/.Xauthority
setopt interactivecomments multios notify

# Autocompletion
autoload -Uz compinit && compinit
compinit -d $HOME/.zcompdump
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

# Dirstack
DIRSTACKSIZE=10
setopt autocd autopushd pushdminus pushdsilent pushdtohome

# Editor
bindkey -e
bindkey '^[h' backward-delete-word
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

# History
export HISTFILE=$HOME/.zhistory
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE
setopt histignoredups histignorespace histsavenodups sharehistory

# Prompt
[[ "$TERM" == 'linux' ]] && precmd() { echo -en "\e[?6c"; }
PS1="[%~]-%# "

# Aliases
alias ls='ls -p --color=auto --group-directories-first'
alias ll='ls -ahlp --color=auto --group-directories-first'
alias la='ls -ap --color=auto --group-directories-first'
alias -g grep='grep --color=auto'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias su='su -'
alias sudo='sudo -E'
alias tmat='tmux attach'
alias xemacs='startx ~/.xinitrc emacs'
