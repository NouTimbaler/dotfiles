#! /bin/sh

# this scripts logs the name and location of new usb storage devices plugged in
# for this to work you have to add the following line as a udev rule:
#
# ACTION=="add", KERNEL=="sd*", RUN+="/home/noutimbaler/.local/bin/log_usb_plugged.sh"

if [ "${ACTION}" = "add" ] && [ -b "${DEVNAME}" ] && [ "${DEVTYPE}" = "disk" ]
then
	    echo "${DEVNAME};${ID_MODEL}" >> /tmp/usbplugged.log
fi
