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

source ${HOME}/.zsh-util
source ${HOME}/.zsh-git
source ${HOME}/.zsh-completion
source ${HOME}/.zsh-param-opts
source ${HOME}/.zsh-zle
source ${HOME}/.zsh-alias
# completion
# compdef pacman-color=pacman

PROMPT=$'$PR_STITLE%{${fg[cyan]}%}$SHLVL%{${fg_bold[cyan]}%}-> %{${fg_no_bold[cyan]}%}%60<...<%~%<<
%{${fg_bold[yellow]}%}%n%{${fg_bold[blue]}%}@%m$(getGitPrompt)%{$reset_color%} '
RPROMPT=$'%{${fg_no_bold[red]}%}%(?..(%?%)) %{$fg_no_bold[blue]%}$(git_prompt_info)%{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{${fg_bold[magenta]}%}\u29f0"
ZSH_THEME_GIT_PROMPT_ADDED="\u2963"
ZSH_THEME_GIT_PROMPT_AHEAD="%{${fg_bold[magenta]}%}\u2963"
ZSH_THEME_GIT_PROMPT_CLEAN="%#"

stty discard undef
archey -c
