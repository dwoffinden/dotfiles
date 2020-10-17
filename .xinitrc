#!/bin/bash

status-notifier-watcher &
compton --backend=glx --paint-on-overlay &

stack exec taffybar &
stack exec -- xmonad --recompile && exec xmonad

