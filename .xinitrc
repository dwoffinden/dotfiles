#!/bin/bash

status-notifier-watcher &
compton --backend=glx --paint-on-overlay &
urxvtd -q -o &

stack exec taffybar &
stack exec -- xmonad --recompile && xmonad

