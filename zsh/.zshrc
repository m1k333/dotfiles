## ~/.zshrc

## Autocompletion
autoload -Uz compinit && compinit
compinit -d $HOME/.zcompdump
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

## Beep OFF; one of these should work
[[ "$TERM" == 'linux' ]] && setterm -blength 0
set bell-style none
unsetopt beep

## Dirstack
DIRSTACKSIZE=10
setopt autocd autopushd pushdminus pushdsilent pushdtohome

## Editor
bindkey -e
bindkey '^[h' backward-delete-word
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

## History
export HISTFILE=$HOME/.zhistory
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE
setopt histignoredups histignorespace histsavenodups sharehistory

## Interactive settings
setopt interactivecomments multios notify

## Prompt
[[ "$TERM" == 'linux' ]] && precmd() { echo -en "\e[?6c"; }
PS1="[%~]-%# "

## Xorg
export XAUTHORITY=$HOME/.Xauthority

## Aliases

# Coreutils
alias ls='ls -p --color=auto --group-directories-first'
alias ll='ls -ahlp --color=auto --group-directories-first'
alias la='ls -ap --color=auto --group-directories-first'
alias -g grep='grep --color=auto'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# Emacs
function emcd
{
    if [[ "$*" == 'start' ]]
    then
        emacs --daemon
        return $?
    elif [[ "$*" == 'kill' ]]
    then
        emacsclient --eval '(kill-emacs)'
        return $?
    else
        echo "Usage: emcd (start|kill)"
        return 1
    fi
}
alias emc='emacsclient -c'
alias xemacs='startx ~/.xinitrc emacs'

# Misc
alias sudo='sudo -E'
alias tmat='tmux attach'

## EOF
