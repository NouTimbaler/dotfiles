#!/bin/sh

# This is bound to Shift+PrintScreen by default, requires maim. It lets you
# choose the kind of screenshot to take, including copying the image or even
# highlighting an area to copy. scrotcucks on suicidewatch right now.

option="$(printf "a selected area\\ncurrent window\\nfull screen\\na selected area (copy)\\ncurrent window (copy)\\nfull screen (copy)" | dmenu -fn "-15" -m 0 -x 875 -y 35 -z 500 -bw 3 -l 6 -i -p "Screenshot which area?")" || exit 1

case $option in
	"a selected area") maim -s ~/Screenshots/pic-"$(date '+%y%m%d-%H%M-%S')-selected.png" ;;
	"current window") maim -i "$(xdotool getactivewindow)" ~/Screenshots/pic-"$(date '+%y%m%d-%H%M-%S')-window.png" ;;
	"full screen") maim ~/Screenshots/pic-"$(date '+%y%m%d-%H%M-%S')-full.png" ;;
	"a selected area (copy)") maim -s | xclip -selection clipboard -t image/png ;;
	"current window (copy)") maim -i "$(xdotool getactivewindow)" | xclip -selection clipboard -t image/png ;;
	"full screen (copy)") maim | xclip -selection clipboard -t image/png ;;
esac

notify-send "Maim" "Screenshot Taken"

