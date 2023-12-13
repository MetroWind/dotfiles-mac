#!/bin/zsh

function randomTheme()
{
    curl -L https://github.com/kovidgoyal/kitty-themes/raw/master/themes.json \
         2>/dev/null | \
        fgrep '"name":' | \
        sed -E 's/ +"name": "(.*)",/\1/g' | \
        sort --random-sort | \
        head -n 1
}

THEME="$(randomTheme)"
echo "New theme: ${THEME}"
kitten themes --config-file-name user.conf "${THEME}"
