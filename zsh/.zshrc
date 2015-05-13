## ~/.zshrc ############################################################

## Autocompletion ######################################################
autoload -Uz compinit && compinit
compinit -d $HOME/.zcompdump
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
setopt completealiases correct extendedglob globdots nocaseglob

## Beep OFF; one of these should work ##################################
[[ "$TERM" == 'linux' ]] && setterm -blength 0
set bell-style none
unsetopt beep

## Dirstack ############################################################
DIRSTACKSIZE=10
setopt autocd autopushd pushdminus pushdsilent pushdtohome

## History #############################################################
HISTFILE=$HOME/.zhistory
HISTSIZE=1000
SAVEHIST=$HISTSIZE
setopt histignoredups histignorespace histsavenodups sharehistory

## Interactive settings ################################################
setopt interactivecomments multios notify

## Editor settings #####################################################

# Don't wait long for key sequences
KEYTIMEOUT=1

# Vi mode settings
function zsh-vi-mode
{
    # Vim-like 'ge' behaviour
    function vi-backward-word-end
    {
        zle vi-forward-word-end
        zle vi-backward-word -n 2 && zle vi-forward-word-end
    }

    # Vim-like 'GE' behaviour
    function vi-backward-blank-word-end
    {
        zle vi-forward-blank-word-end
        zle vi-backward-blank-word -n 2 \
            && zle vi-forward-blank-word-end
    }

    # Vim-like keybindings; '-v': insert, '-a': command
    bindkey -v
    bindkey -v 'ge'   vi-backward-word-end
    bindkey -v 'GE'   vi-backward-blank-word-end
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


    # Right prompt '(CMD)' in command mode; be sure to
    # reset 'RPROMPT' in precmd (see 'Prompt' section)
    function zle-keymap-select
    {
        RPROMPT=''
        [[ $KEYMAP = vicmd ]] && RPROMPT="(CMD)"
        function { return $__prompt_status; }
        zle reset-prompt
    }

    # Preserve return statuses
    function zle-line-init
    { typeset -g __prompt_status="$?"; }

    # Load the functions defined above
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

    # Emacs-like keybindings; '-e': emacs 
    bindkey -e
    bindkey -e '^[h'  backward-delete-word
    bindkey -e '\e[A' history-beginning-search-backward
    bindkey -e '\e[B' history-beginning-search-forward
}

# Pick an editor mode
zsh-emacs-mode

## Prompt ##############################################################

# Prompt definition
PROMPT="[%~]-%# "

# Prompt settings
if [[ "$TERM" == 'linux' ]] ; then
    # Send escape char to get block cursor in linux console
    function precmd { RPROMPT=''; echo -en "\e[?6c"; }
else
    # Empty RPROMPT is needed for zle's vi mode indication
    function precmd { RPROMPT=''; }
fi

## Aliases #############################################################

# Coreutils and friends
alias cp='cp -i'
alias grep='grep --color=auto'
alias la='ls -ap --color=auto --group-directories-first'
alias ll='ls -ahlp --color=auto --group-directories-first'
alias ls='ls -p --color=auto --group-directories-first'
alias mv='mv -i'
alias rm='rm -i'
alias sudo='sudo -E'

# Emacs
alias emc='emacsclient -c'
alias emcd='emacs --daemon'
alias emcd-kill="emacsclient --eval '(kill-emacs)'"

# Info
alias bat='acpi -V'

# Shell
alias :q='exit'

# Tmux
alias tmat='tmux attach || tmux new-session'

# X server
alias xbspwm='startx ~/.xinitrc bspwm'
alias xstumpwm='startx ~/.xinitrc stumpwm'

## EOF #################################################################
