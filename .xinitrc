#!/bin/sh

compton --backend=glx --paint-on-overlay &
urxvtd -q -o -f

stack exec -- xmonad --recompile && exec xmonad
