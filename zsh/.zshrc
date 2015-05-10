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

# Global keybindings
KEYTIMEOUT=1
bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

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

    # Vim-like keybindings
    bindkey -v
    bindkey '^G' what-cursor-position
    bindkey -a 'gg' beginning-of-buffer-or-history
    bindkey -a 'g~' vi-oper-swap-case
    bindkey -a G end-of-buffer-or-history
    bindkey -a u undo
    bindkey -a '^R' redo
    bindkey -v '^?' backward-delete-char
    bindkey -v '^H' backward-delete-char
    bindkey -v 'ge' vi-backward-word-end
    bindkey -v 'GE' vi-backward-blank-word-end

    # Right prompt '(CMD)' in command mode; be sure to
    # reset 'RPROMPT' in precmd (see 'Prompt' section)
    function zle-keymap-select
    {
        RPROMPT=""
        [[ $KEYMAP = vicmd ]] && RPROMPT="(CMD)"
        () { return $__prompt_status }
        zle reset-prompt
    }

    # Preserve return statuses
    function zle-line-init
    { typeset -g __prompt_status="$?" }

    # Load the functions defined above
    zle -N zle-keymap-select
    zle -N zle-line-init
}

# Emacs mode settings
function zsh-emacs-mode
{
    # Get rid of vi mode behaviour if it has been defined
    zle -D zle-keymap-select
    zle -D zle-line-init 
    bindkey '^G' list-expand

    # Emacs-like keybindings
    bindkey -e
    bindkey -e '^[h' backward-delete-word
}

# Pick an editor mode
zsh-vi-mode

## Prompt ##############################################################

# Set up prompt
if [[ "$TERM" == 'linux' ]] ; then
    # Send escape char to get block cursor in linux console
    function precmd { RPROMPT=""; echo -en "\e[?6c"; }
else
    # 'RPROMPT=""' is always needed for zle in vi mode
    function precmd { RPROMPT=""; }
fi

# Define left prompt
PROMPT="[%~]-%# "

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
alias xemacs='startx ~/.xinitrc emacs'

# Tmux
alias tmat='tmux attach'

## EOF #################################################################
