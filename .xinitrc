#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#xrdb -merge ~/.Xresources

exec systemd --user

#[ -n "$1" ] && wm="$1" || wm='xmonad'

#exec $wm
