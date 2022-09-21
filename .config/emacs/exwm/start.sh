#!/bin/sh

picom &

xss-lock -- slock &

export VISUAL=emacsclient
export EDITOR="$VISUAL"

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/exwm/desktop.el
