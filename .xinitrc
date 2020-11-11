#!/bin/bash

status-notifier-watcher &
compton --backend=glx --paint-on-overlay &

TAFFYBAR=~/.local/bin/taffybar-built
XMONAD=~/.local/bin/xmonad-built

[[ -f $TAFFYBAR ]] ||
    (cd ~/.config/taffybar && stack ghc -- -threaded taffybar.hs -o $TAFFYBAR)

$TAFFYBAR &

[[ -f $XMONAD ]] ||
    (cd ~/.xmonad && stack ghc xmonad.hs -- -o $XMONAD)

exec $XMONAD

