#!/bin/zsh

## Initializations #############################################################

# Pre-prompt command will run $PROMPT_COMMAND; temporarily make $PROMPT blank,
# because zsh will load the prompt after precmd, and we're re-loading it later
# anyway -- better to not do it twice.
PROMPT_COMMAND=''
function precmd { eval "$PROMPT_COMMAND" }

# Parse current git branch or return empty string
prompt_git_branch=''
function current-git-branch
{
    local git_branch=$(git branch | grep '\*' | sed 's/\* //')
    [ -n "$git_branch" ] && \
        prompt_git_branch="[${git_branch}]~" || prompt_git_branch=''
}

# Add git branch to prompt if git is installed
command -v git &> /dev/null && \
    PROMPT_COMMAND="${PROMPT_COMMAND} ; current-git-branch &> /dev/null"

## Settings ####################################################################

## Autocompletion
autoload -Uz compinit && compinit -d $HOME/.zcompdump
zmodload zsh/complist
zstyle ':completion:*' menu select 
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

## Linux vconsole-specific options
if [ $TERM = linux ]
then
    # Beep off - these should cover all cases
    setterm -blength 0
    set bell-style none
    unsetopt beep
    # Send escape to get a block cursor
    PROMPT_COMMAND="${PROMPT_COMMAND} ; echo -en \"\e[?6c\""
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
        PROMPT="${prompt_git_branch}${PROMPT_STATIC}${VIMODE} "
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
    # Emacs-like keybindings; '-e': emacs 
    bindkey -e
    bindkey -e '^[h'  backward-delete-word
    bindkey -e '\e[A' history-beginning-search-backward
    bindkey -e '\e[B' history-beginning-search-forward
    bindkey -M menuselect '^[[Z' reverse-menu-complete

    # Prompt -- simpler than in vi mode
    function zle-keymap-select zle-line-init
    {
        PROMPT="${prompt_git_branch}${PROMPT_STATIC}%F{magenta}%#%f "
        typeset -g __prompt_status="$?"
        function { return $__prompt_status }
        zle reset-prompt
    }
    zle -N zle-keymap-select
    zle -N zle-line-init
}

## Prompt ######################################################################

# Prompt definition
autoload -U colors && colors
PROMPT_STATIC="(%n@%M)~(%~)~(%?)~"

# Syntax highlighting plugin
SYNTAXHLFILE="/usr/share/zsh/plugins/zsh-syntax-highlighting"
SYNTAXHLFILE="${SYNTAXHLFILE}/zsh-syntax-highlighting.zsh"
[ -f $SYNTAXHLFILE ] && source $SYNTAXHLFILE

# Pick an editor mode
zsh-vi-mode

## Aliases and functions #######################################################

# GNU system-specific options
if ls --version &> /dev/null | grep coreutils &> /dev/null
then
    LS_GNUOPTS='--color=auto --group-directories-first '
    GREP_GNUOPTS='--color=auto '
    eval $(dircolors -b)
fi

# coreutils and friends
alias ls="ls ${LS_GNUOPTS}-p"
alias la="ls ${LS_GNUOPTS}-ap"
alias ll="ls ${LS_GNUOPTS}-hlp"
alias lal="ls ${LS_GNUOPTS}-ahlp"
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias mkdir='mkdir -pv'
alias grep="grep ${GREP_GNUOPTS}"
alias sush="sudo -E ${SHELL}"
alias sued="sudoedit"

# Applications
alias emc='emacs -nw'
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

## EOF #########################################################################
