#! /bin/sh

# this scrips notifies the user when a new usb is plugged in
# should be runned at background and at startup

log_file="/tmp/usbplugged.log"

send_notification() {
	if [ -f $log_file ]
	then
		line=$(tail -1 $log_file)
		loc=$(echo $line | cut -d ";" -f1)
		nom=$(echo $line | cut -d ";" -f2)

		notify-send "New USB device detected" "New device\n$nom\ndetected at\n$loc"
	fi
}

inotifywait -q -m -e modify $log_file |
while read -r filename event; do
	send_notification
done

