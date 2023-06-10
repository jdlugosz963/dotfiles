#!/bin/sh

# picom &

xss-lock -- slock &

export VISUAL="emacsclient -a vim"
export EDITOR="$VISUAL"
export GTK_THEME="Adwaita:dark"
export CALIBRE_USE_DARK_PALETTE=1
export JD_EXWM=1

gentoo-pipewire-launcher &

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/exwm/desktop.el
