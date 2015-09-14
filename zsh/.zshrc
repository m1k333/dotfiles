#!/bin/zsh

##
## Profile
##

. ${HOME}/.zprofile

##
## Basic configuration
##

# Line editor mode: 'emacs' or 'vi'
EDITOR_MODE='emacs'

# Prompt definition: ${PROMPT_STATIC} is the basic prompt
autoload -U colors && colors
PROMPT_STATIC="(%n@%M)~(%~)~(%?)~"

# Syntax highlighting plugin: point this to the correct location
SYNTAXHLFILE='/usr/share/zsh/plugins/zsh-syntax-highlighting'
SYNTAXHLFILE="${SYNTAXHLFILE}/zsh-syntax-highlighting.zsh"
test -f ${SYNTAXHLFILE} && source ${SYNTAXHLFILE}

# Autocompletion
autoload -Uz compinit && compinit -d ${HOME}/.zcompdump
zmodload zsh/complist
zstyle ':completion:*' menu select 
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

# Dirstack
DIRSTACKSIZE=10
setopt autocd autopushd pushdminus pushdsilent pushdtohome

# History files
HISTFILE=${HOME}/.zhistory
HISTSIZE=1000
SAVEHIST=${HISTSIZE}
setopt histignoredups histignorespace histsavenodups sharehistory

# Interaction
KEYTIMEOUT=1
setopt interactivecomments multios notify

##
## Prompt initialization
##

# Pre-prompt command will be to eval "${PROMPT_COMMAND}"
function precmd { eval "${PROMPT_COMMAND}" }

# Set behaviour of the linux console if we're there
if test "${TERM}" = 'linux'
then
    # Beep off - these should cover all cases
    #setterm -blength 0
    #set bell-style none
    #unsetopt beep
    # Send escape to get a block cursor
    PROMPT_COMMAND="${PROMPT_COMMAND} ; echo -en \"\e[?6c\""
fi

# If git is installed, integrate it into the prompt
if command -v git &> /dev/null
then
    function precmd_git_branch
    {
        PROMPT_GIT_BRANCH=''
        if test -d './.git'
        then
            local GIT_STRING="$(git branch | grep '\*' | sed 's/\* //')"
            test -n "${GIT_STRING}" && PROMPT_GIT_BRANCH="[${GIT_STRING}]~"
        fi
    }
    PROMPT_COMMAND="${PROMPT_COMMAND} ; precmd_git_branch &> /dev/null"
fi

##
## Emacs line editor mode
##

function zsh-emacs-mode
{
    # Emacs-like keybindings; '-e': emacs 
    bindkey -e
    bindkey -e '^[h'  backward-delete-word
    bindkey -e '\e[A' history-beginning-search-backward
    bindkey -e '\e[B' history-beginning-search-forward
    bindkey -M menuselect '^[[Z' reverse-menu-complete

    # Prompt; needs to be updated on line init, after precmd finishes
    function zle-keymap-select zle-line-init
    {
        PROMPT="${PROMPT_GIT_BRANCH}${PROMPT_STATIC}%F{magenta}%#%f "

        # Preserve return status across line inits
        typeset -g __prompt_status=${?}
        function { return ${__prompt_status} }
        zle reset-prompt
    }

    # Reload the functions defined above
    zle -N zle-keymap-select
    zle -N zle-line-init
}

##
## Vi line editor mode
## 

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

    # Behaviour when initializing prompt or changing vi mode
    function zle-keymap-select zle-line-init
    {
        # Prompt; needs to be updated on line init, after precmd finishes;
        # '%': insert mode, '@': command mode
        local VIMODE
        test "${KEYMAP}" = 'vicmd'  \
            && VIMODE="%F{blue}@%f" \
            || VIMODE='%F{magenta}%#%f'
        PROMPT="${PROMPT_GIT_BRANCH}${PROMPT_STATIC}${VIMODE} "

        # Preserve return status across line inits and vi mode changes
        typeset -g __prompt_status=${?}
        function { return ${__prompt_status} }
        zle reset-prompt
    }

    # Reload the functions defined above
    zle -N zle-keymap-select
    zle -N zle-line-init
}

##
## Finalizing setup
##

# Set up line editor
zsh-${EDITOR_MODE}-mode

##
## Alias and function definition
##

# GNU system-specific options
if ls --version | grep coreutils &> /dev/null
then
    LS_GNUOPTS='--color=auto --group-directories-first '
    GREP_GNUOPTS='--color=auto '
    eval "$(dircolors -b)"
fi

# Coreutils aliases
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

# Application aliases
alias emc='emacs -nw'
alias pacman='pacman --color auto'
alias startx-multihead='startx ~/.xinitrc multihead'
alias tmat='tmux attach || tmux new-session'

# Function to create .bak files
function bak { for i in ${@}; do cp -R ${i} ${i}.bak; done }

## EOF
