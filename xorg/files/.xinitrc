source $HOME/.profile
source $HOME/.xinit-user

export GTK_IM_MODULE=${InputMethod}
export XMODIFIERS=@im=${InputMethod}
export QT_IM_MODULE=${InputMethod}

if ! [ -e "/usr/bin/ibus-daemon" ] && [ "x$XDG_SESSION_TYPE" = "xwayland" ] ; then
  exec env IM_CONFIG_CHECK_ENV=1 im-launch true;
fi

if [[ -e ${Wallpaper} ]]; then
    feh --bg-fill ${Wallpaper}
fi

xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &
