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
source ${HOME}/.zsh/zsh-user

if [ -d ${HOME}/.zsh/zsh-syntax-highlighting ]; then
    source ${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [ -d ${HOME}/.zsh/zsh-autosuggestions ]; then
    source ${HOME}/.zsh/zsh-autosuggestions/autosuggestions.zsh
    # Enable autosuggestions automatically.
    zle-line-init()
    {
        zle autosuggest-start
    }
    zle -N zle-line-init
fi

# completion
# compdef pacman-color=pacman

PROMPT=$'%F{cyan}$SHLVL-> %60<...<%~\n%F{yellow}%n%F{blue}@%m%f$(getGitPrompt) '
RPROMPT=$'%{${fg_no_bold[red]}%}%(?..(%?%)) %{$fg_no_bold[blue]%}$(git_prompt_info)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{${fg_bold[magenta]}%}\u29f0"
ZSH_THEME_GIT_PROMPT_ADDED="\u2963"
ZSH_THEME_GIT_PROMPT_AHEAD="%{${fg_bold[magenta]}%}\u2963"
ZSH_THEME_GIT_PROMPT_CLEAN="%#"

stty discard undef
