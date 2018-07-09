#!/usr/bin/env bash

if command -v keychain &>/dev/null; then
    keychain --quiet --quick "${HOME}/.ssh/id_rsa"
fi

if command -v unity-settings-daemon 2>/dev/null; then
    if [ -z "$(pgrep -u "${USER}" -f unity-settings-daemon)" ]; then
        unity-settings-daemon &
    fi
elif command -v gnome-settings-daemon 2>/dev/null; then
    if [ -z "$(pgrep -u "${USER}" gnome-settings-daemon)" ]; then
         gnome-settings-daemon &
    fi
fi

if [ -z "$(pgrep -u "${USER}" -f xscreensaver)" ]; then
    if [ -n "$(pgrep -u "${USER}" -f gnome-screensaver)" ]; then
        kill -9 "$(pgrep -u "${USER}" -f gnome-screensaver)"
    fi
    xscreensaver -no-splash &
fi

if [ -z "$(pgrep -u "${USER}" nm-applet)" ]; then
    nm-applet &
fi

if [ -z "$(pgrep -u "${USER}" taffybar)" ]; then
    taffybar &
fi

if [ -x "${HOME}/.local/bin/startup-hook.local.sh" ]; then
    "${HOME}"/.local/bin/startup-hook.local.sh
fi
