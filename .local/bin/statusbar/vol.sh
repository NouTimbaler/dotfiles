#!/bin/sh

# Prints the current volume or 🔇 if muted.


vol="$(wpctl get-volume @DEFAULT_AUDIO_SINK@)"
muted="$(echo $vol | cut -d ' ' -f3)"
num="$(echo $vol | cut -d ' ' -f2 | cut -d '.' -f2)"

[ $muted = "[MUTED]" ] && echo "󰖁 " && exit

if [ "$num" -gt "70" ]; then
  icon=" "
elif [ "$num" -lt "30" ]; then
  icon=" "
else
  icon=" "
fi

echo "$icon$num%"
