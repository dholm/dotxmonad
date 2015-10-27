#!/usr/bin/env bash

if hash keychain &>/dev/null; then
    keychain --quiet --quick "${HOME}/.ssh/id_rsa"
fi

if [ -z "$(pgrep -u ${USER} gnome-settings-daemon)" ]; then
    gnome-settings-daemon &
fi

if [ -z "$(pgrep -u ${USER} xscreensaver)" ]; then
    if [ -n "$(pgrep -u ${USER} gnome-screensaver)" ]; then
        killall -9 gnome-screensaver
    fi
    xscreensaver -no-splash &
fi

if [ -z "$(pgrep -u ${USER} nm-applet)" ]; then
    nm-applet &
fi

if [ -z "$(pgrep -u ${USER} taffybar)" ]; then
    taffybar &
fi
