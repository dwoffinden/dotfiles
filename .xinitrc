#!/bin/bash

compton --backend=glx --paint-on-overlay &
urxvtd -q -o &

stack exec -- xmonad --recompile && xmonad
