#!/usr/bin/bash

case $2 in
    DEINSTALL)
	echo "Stopping Wiggle service."
	svcadm disable network/wiggle
	;;
    POST-DEINSTALL)
	echo "Removing Wiggle service."
	svccfg delete network/wiggle
	echo "Please beware that database and logfiles have not been"
	echo "deleted! Neither have the wiggle user or gorup."
	echo "If you don't need them any more remove the directories:"
	echo " /var/log/wiggle"
	;;
esac
