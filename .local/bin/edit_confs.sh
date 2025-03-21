#!/bin/sh

find ~/.local/bin/ ~/.config/ -maxdepth 2 -type f -print | dmenu -fn '-20' -i -l 10 -c -bw 3 -p "Choose a conf to edit" | xargs -r $TERMINAL -e vim 

