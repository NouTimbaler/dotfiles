#!/bin/sh

int=""

if grep -xq 'up' /sys/class/net/w*/operstate 2>/dev/null ; then
		wifiicon="󰖩"
        int="$(awk '/^\s*w/ { print int($3 * 100 / 70) }' /proc/net/wireless)"
        printf "%s%3s%s" "$wifiicon " "$int" "%"
	elif grep -xq 'down' /sys/class/net/w*/operstate 2>/dev/null ; then
			grep -xq '0x1003' /sys/class/net/w*/flags && wifiicon=" " || wifiicon="❌ "
            printf "%s" "$wifiicon"

fi

printf "%s%s\n" "$(sed "s/down/󰈂/;s/up/󰈁/" /sys/class/net/e*/operstate 2>/dev/null)" "$(sed "s/.*//" /sys/class/net/tun*/operstate 2>/dev/null)"

