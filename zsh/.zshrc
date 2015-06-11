#!/bin/zsh

## Settings ####################################################################

## Autocompletion
autoload -Uz compinit && compinit -d $HOME/.zcompdump
zmodload zsh/complist
zstyle ':completion:*' menu select 
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

if [ $TERM = linux ]
then
    ## Beep OFF; one of these should work
    setterm -blength 0
    set bell-style none
    unsetopt beep
    # Send escape to get a block cursor
    function precmd { echo -en "\e[?6c" }
fi

## Dirstack
DIRSTACKSIZE=10
setopt autocd autopushd pushdminus pushdsilent pushdtohome

## History
HISTFILE=$HOME/.zhistory
HISTSIZE=1000
SAVEHIST=$HISTSIZE
setopt histignoredups histignorespace histsavenodups sharehistory

## Interactive settings
setopt interactivecomments multios notify

## Editor settings #############################################################

# Don't wait long for key sequences
KEYTIMEOUT=1

# Vi mode settings
function zsh-vi-mode
{
    # Vim-like keybindings; '-v': insert, '-a': command
    bindkey -v
    bindkey -v '^G'   what-cursor-position
    bindkey -v '^H'   backward-delete-char
    bindkey -v '^?'   backward-delete-char
    bindkey -v '\e[A' history-beginning-search-backward
    bindkey -v '\e[B' history-beginning-search-forward
    bindkey -a 'gg'   beginning-of-buffer-or-history
    bindkey -a 'g~'   vi-oper-swap-case
    bindkey -a 'G'    end-of-buffer-or-history
    bindkey -a '^G'   what-cursor-position
    bindkey -a '^R'   redo
    bindkey -a 'u'    undo
    bindkey -a '\e[A' history-beginning-search-backward
    bindkey -a '\e[B' history-beginning-search-forward
    bindkey -M menuselect '^[[Z' reverse-menu-complete

    # Behaviour when initializing prompt or changing vi mode:
    # Prompt reflects vi mode; vi mode change preserves return status
    function zle-keymap-select zle-line-init
    {
        [ $KEYMAP = vicmd ] && VIMODE="%F{blue}@%f" || VIMODE='%F{magenta}%#%f'
        PROMPT="${PROMPT_STATIC}${VIMODE} "
        typeset -g __prompt_status="$?"
        function { return $__prompt_status }
        zle reset-prompt
    }
    zle -N zle-keymap-select
    zle -N zle-line-init
}

# Emacs mode settings
function zsh-emacs-mode
{
    # Get rid of vi mode zle functions if they have been defined --
    # (re)initialize them with placeholder values, then delete them
    function blank {}
    zle -N zle-keymap-select blank
    zle -N zle-line-init blank
    zle -D zle-keymap-select 
    zle -D zle-line-init 
    unfunction blank

    # Emacs-like keybindings; '-e': emacs 
    bindkey -e
    bindkey -e '^[h'  backward-delete-word
    bindkey -e '\e[A' history-beginning-search-backward
    bindkey -e '\e[B' history-beginning-search-forward
    bindkey -M menuselect '^[[Z' reverse-menu-complete

    # Prompt -- simpler than in vi mode
    PROMPT="${PROMPT_STATIC}%F{magenta}%#%f "
}

## Prompt ######################################################################

# Prompt definition
autoload -U colors && colors
PROMPT_STATIC="[%~]~(%?)~"

# Syntax highlighting plugin
SYNTAXHLFILE="/usr/share/zsh/plugins/zsh-syntax-highlighting"
SYNTAXHLFILE="${SYNTAXHLFILE}/zsh-syntax-highlighting.zsh"
[ -f $SYNTAXHLFILE ] && source $SYNTAXHLFILE

# Pick an editor mode
zsh-vi-mode

## Aliases and functions #######################################################

# GNU system-specific options
if ls --version | grep coreutils &> /dev/null
then
    LS_GNUOPTS='--color=auto --group-directories-first '
    GREP_GNUOPTS='--color=auto '
    eval $(dircolors -b)
fi

# coreutils and friends
alias l="ls ${LS_GNUOPTS}-1p"
alias ls="ls ${LS_GNUOPTS}-p"
alias la="ls ${LS_GNUOPTS}-ap"
alias ll="ls ${LS_GNUOPTS}-ahlp"
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias mkdir='mkdir -pv'
alias grep="grep ${GREP_GNUOPTS}"
alias sush="sudo -E ${SHELL}"
alias sued="sudoedit"

# Applications
alias pacman='pacman --color auto'
alias tmat='tmux attach || tmux new-session'

# Make .bak files
function bak
{
    for i in $@
    do
        cp -R ${i} ${i}.bak
    done
}

# Neovim testing environment
function vim-env
{
    if [ $1 = vim ]
    then
        function { export EDITOR=vim && unalias vim }        \
        && echo "\nOk. 'vim' runs original vim.\n"           \
        || echo "\nError. Try resetting manually.\n" 1>&2
    elif [ $1 = nvim ]
    then
        function { export EDITOR=nvim && alias vim=nvim }    \
        &&  echo "\nOk. 'vim' runs neovim.\n"                \
        || function { echo "\nError. Resetting env:\n" 1>&2
                      vim-env vim } 
    else
        echo "\nUsage: vim-env (vim|nvim).\n"
        return 1
    fi
}

## EOF #########################################################################
