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
xsetroot -cursor_name left_ptr
#trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha $(echo "`expr 255 - $(cat ~/.xmobar/xmobarrc | grep alpha | cut -d "=" -f2 | cut -d " " -f2)`") --tint $coul --expand true --heighttype request --height 23 --padding 1 &
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 100 --tint 0x000000 --height 16 &
xfce4-power-manager &
nm-applet &
redshift &
feh --randomize --bg-scale /usr/share/backgrounds/* &
xbacklight -set 35
taffybar &

exec ck-launch-session dbus-launch $HOME/.cabal/bin/xmonad
