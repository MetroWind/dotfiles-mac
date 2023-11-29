source ${HOME}/.zsh/system

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

source ${HOME}/.zsh/zsh-util
source ${HOME}/.zsh/zsh-git
source ${HOME}/.zsh/zsh-completion
source ${HOME}/.zsh/zsh-param-opts
source ${HOME}/.zsh/zsh-zle
source ${HOME}/.zsh/zsh-alias

if [[ -e ${HOME}/.zsh/zsh-user ]]; then
    source ${HOME}/.zsh/zsh-user
fi

if [[ -e ${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source ${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [[ -e ${HOME}/.zsh/zsh-autosuggestions/autosuggestions.zsh ]]; then
    source ${HOME}/.zsh/zsh-autosuggestions/autosuggestions.zsh
    # Enable autosuggestions automatically.
    zle-line-init()
    {
        zle autosuggest-start
    }
    zle -N zle-line-init
fi

# Exa colors, requires 24 bit color.
export EXA_COLORS="da=38;2;108;107;135:uu=0;33:gu=0;33"

# completion
# compdef pacman-color=pacman

if (( ${+commands[oh-my-posh]} )); then
    if [ "$TERM_PROGRAM" != "Apple_Terminal" ]; then
        if (( ${+commands[brew]} )); then
            # If OME_THEME is not defined, use a default theme.
            if [[ ! -v OMP_THEME ]]; then
                # OMP_THEME="amro“”
                # OMP_THEME="wopian"
                OMP_THEME="pure"
            fi
            eval "$(oh-my-posh init zsh --config $(brew --prefix oh-my-posh)/themes/${OMP_THEME}.omp.json)"
        else
            # If there’s no homebrew, the theme is taken from a URL.
            if [[ ! -v OMP_THEME ]]; then
                OMP_THEME="https://github.com/JanDeDobbeleer/oh-my-posh/blob/main/themes/amro.omp.json"
            fi
            eval "$(oh-my-posh init zsh --config ${OMP_THEME})"
        fi
    fi
else

    PROMPT=$'%F{cyan}$SHLVL ‡ %60<...<%~\n%F{yellow}%n%F{blue}@%m%f$(getGitPrompt) '
    RPROMPT=$'%{${fg_no_bold[red]}%}%(?..(%?%)) %{$fg_no_bold[blue]%}$(git_prompt_info)%{$reset_color%}'

    ZSH_THEME_GIT_PROMPT_PREFIX=""
    ZSH_THEME_GIT_PROMPT_SUFFIX=""
    ZSH_THEME_GIT_PROMPT_DIRTY="%{${fg_bold[magenta]}%}\u2260"
    ZSH_THEME_GIT_PROMPT_ADDED="\u0444"
    ZSH_THEME_GIT_PROMPT_AHEAD="%{${fg_bold[magenta]}%}\u2642"
    ZSH_THEME_GIT_PROMPT_CLEAN="%#"
fi

if (( ${+commands[fastfetch]} )); then
    fastfetch -c neofetch
elif (( ${+commands[neofetch]} )); then
    neofetch
fi

# Welcome message
if [[ -e $HOME/.motd ]]; then
    print -n "${fg[red]}"
    cat $HOME/.motd
    print "$reset_color"
    uptime
fi

stty discard undef
