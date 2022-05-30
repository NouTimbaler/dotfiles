#!/bin/sh


find ~/.local/bin/ ~/.config/ -maxdepth 2 -type f -print | dmenu -i -l 10 -c -bw 2 -p "Choose a conf to edit" | xargs -r $TERMINAL -e nvim 
