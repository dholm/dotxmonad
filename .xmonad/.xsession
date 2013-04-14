#!/bin/sh

[[ -f $HOME/.Xdefaults ]] && xrdb -merge $HOME/.Xdefaults

exec gnome-session --session xmonad-gnome "$@"
