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

#xrandr --newmode "1440x900_60.00" 106.50 1440 1528 1672 1904 900 903 909 934 -Hsync +Vsync
#xrandr --addmode DVI-I-2 1440x900_60.00
#xrandr --output DVI-I-2 --mode 1440x900_60.00 --right-of DVI-I-1

#xrandr --output LVDS --mode 1366x768
#xrandr --output VGA-0 --mode 1920x1080 --rate 60 --right-of LVDS

#if [[ -n "$2" ]] ; then
#  unit=$2
#else
#  unit='xmonad.target'
#fi
#
#exec /usr/lib/systemd/systemd --user --unit=$unit

[ -n "$1" ] && wm="$1" || wm='/usr/bin/xmonad'
exec $wm
