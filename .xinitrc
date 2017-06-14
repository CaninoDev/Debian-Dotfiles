#!/bin/sh

#. /etc/X11/xinit/*

#if [ -d /etc/X11/xinit ]; then
  #for f in /etc/X11/xinit/*; do
    #[ -x "$f" ] && . "$f"
  #done
  #unset f
#fi
xrdb -merge ~/.Xresources
xset +fp ~/.local/share/fonts/NerdFonts
xset +fp ~/.local/share/fonts/Apple/OpenType/San\ Francisco\ Display
xset +fp rehash
xsetroot -cursor_name left_ptr
xfce4-power-manager &
nm-applet &
redshift &
feh --randomize --bg-scale /usr/share/backgrounds/* &
xbacklight -set 35
compton -b

exec ck-launch-session dbus-launch $HOME/.cabal/bin/xmonad
