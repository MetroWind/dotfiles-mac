export OSFONTDIR="$HOME/Library/Fonts;/Library/Fonts"
export OSCOLORDIR="/System/Library/ColorSync/Profiles;/Library/Application Support/Adobe/Color"
export PATH="/usr/local/sbin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"
export PATH="$HOME/bin:$PATH"
export INFOPATH="$HOME/.emacs.d/info:$INFOPATH"
export EDITOR="/usr/local/bin/emacsclient"
export PAGER="most"
export HOMEBREW_CC="clang"

# Python
export VIRTUAL_ENV2="$HOME/Python2"
export VIRTUAL_ENV3="$HOME/Python3"
export PATH="${VIRTUAL_ENV3}/bin:${VIRTUAL_ENV2}/bin:$PATH"
export PYTHONPATH="$HOME/.Python"

# Node.js
export PATH="/usr/local/share/npm/bin:$PATH"
# Go
export PATH="/usr/local/opt/go/libexec/bin/go:$PATH"
# Rustup
export PATH="${HOME}/.cargo/bin:$PATH"

# LilyPond
# export PATH="/Applications/LilyPond.app/Contents/Resources/bin:$PATH"

# GPG agent
if which gpgconf >& /dev/null; then
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
fi
