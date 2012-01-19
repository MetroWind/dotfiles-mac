# See if we can use colors.
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done
PR_NO_COLOUR="%{$terminfo[sgr0]%}"


# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'argu.: %d'
zstyle ':completion:*' completer _expand _complete _match _correct _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format "%{${fg_bold[red]}%}# Completing %d%{${fg_no_bold[default]}%}:"
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z} m:{a-zA-Z}={A-Za-z}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 2 numeric
zstyle ':completion:*' menu select=long
zstyle ':completion:*' prompt "%{${fg_bold[red]}%}# Corrections%{${fg_no_bold[default]}%}: (%e errors)"
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/Users/corsair/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1024
SAVEHIST=1000
# End of lines configured by zsh-newuser-install

WORDCHARS=_-

# Put /usr/local/bin before /usr/bin
LocalBinIndex=${path[(i)/usr/local/bin]}
if [ ${LocalBinIndex} -le ${#path} ]; then
    UsrBinIndex=${path[(i)/usr/bin]}
    path[${LocalBinIndex}]=()
    BeforeUsrBin=
    if [ ${UsrBinIndex} -gt 1 ]; then
        BeforeUsrBin=(${path[1,${UsrBinIndex}-1]})
    fi
    AfterUsrBin=(${path[${UsrBinIndex},-1]}) # Contains /usr/bin
    path=(${BeforeUsrBin} /usr/local/bin ${AfterUsrBin})
fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    alias ls='gls --color=auto -h -X -F --group-directories-first -v'
fi

# Fix Home and End key
# rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line

bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
# # for xterm
# bindkey "\eOH" beginning-of-line
# bindkey "\eOF" end-of-line

# # for Konsole
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line

# Options
setopt MAIL_WARNING
setopt NO_NOTIFY
unsetopt AUTO_CD
setopt INTERACTIVE_COMMENTS
setopt CDABLE_VARS
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt GLOB_COMPLETE
setopt LIST_PACKED
setopt LIST_ROWS_FIRST
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt AUTO_CONTINUE
setopt LONG_LIST_JOBS

# Quickly input `../'
rationalise-dot() {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}
zle -N rationalise-dot

# Aliases
alias ec='emacsclient --no-wait'

# Custom Key Bindings
bindkey "\e;" vi-pound-insert
bindkey . rationalise-dot

# completion
# compdef pacman-color=pacman

export PS1="$PR_STITLE%{${fg[cyan]}%}$SHLVL%{${fg_bold[cyan]}%}-> %{${fg_no_bold[cyan]}%}%60<...<%~%<<
%{${fg_bold[yellow]}%}%n%{${fg_bold[white]}%}@%m%#%{${fg_no_bold[default]}%} "
export RPS1="%{${fg_no_bold[red]}%}%(?..(%?%))%{${fg_no_bold[default]}%}"

source $HOME/.profile
