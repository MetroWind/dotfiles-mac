#!/usr/bin/env zsh

# Usage: $0 SCREENSHOT_FILE OUTPUT
#
# Given a dual-screen shot file, seperate the 2 parts with some small
# offset, and resize.

convert -crop 1440x2560 "$1" '/tmp/halves-%d.png'
convert /tmp/halves-{0,1}.png -background none +smush 50 -resize '50%' "$2"
rm -f /tmp/halves-{0,1}.png
