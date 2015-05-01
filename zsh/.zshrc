# ~/.zshrc

# General options
export XAUTHORITY=$HOME/.Xauthority
setopt interactivecomments multios notify nobeep

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
bindkey="emacs"
if [[ "$bindkey" == "vi" ]]; then
    bindkey -v
    export KEYTIMEOUT=1
    bindkey -a 'gg' beginning-of-buffer-or-history
    bindkey -a 'g~' vi-oper-swap-case
    bindkey -a G end-of-buffer-or-history
    bindkey -a u undo
    bindkey -a '^R' redo
    bindkey '^?' backward-delete-char
    bindkey '^H' backward-delete-char
    bindkey '^G' what-cursor-position
elif [[ "$bindkey" == "emacs" ]]; then
    bindkey -e
fi

# History
export HISTFILE=$HOME/.zhistory
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE
setopt histignoredups histignorespace histsavenodups sharehistory
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward


# Prompt
phashD="%#"
dirind="[%~]-"

if [[ "$bindkey" == "vi" ]]; then

    precmd()
    {
        phashF="${phashD}"
        PS1="${dirind}${phashF} "
    }

    zle-keymap-select()

    {
        phashF="${phashD}"
        [[ $KEYMAP = vicmd ]] && phashF="/"
        PS1="${dirind}${phashF} "
        () { return $__prompt_status }
        zle reset-prompt
    }

    zle-line-init()
    { typeset -g __prompt_status="$?" }

    zle -N zle-keymap-select
    zle -N zle-line-init

else

    PS1="${dirind}${phashD} "

fi

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
alias emt='emacs -nw'
alias tmat='tmux attach'
