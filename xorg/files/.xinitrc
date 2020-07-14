source $HOME/.profile
source $HOME/.xinit-user

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

export XSECURELOCK_COMPOSITE_OBSCURER=0
export XSECURELOCK_SAVER=saver_xscreensaver

ibus-daemon -drx
if ! [ -e "/usr/bin/ibus-daemon" ] && [ "x$XDG_SESSION_TYPE" = "xwayland" ] ; then
  exec env IM_CONFIG_CHECK_ENV=1 im-launch true;
fi

if [[ -e ${Wallpaper} ]]; then
    feh --bg-fill ${Wallpaper}
fi

xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &
