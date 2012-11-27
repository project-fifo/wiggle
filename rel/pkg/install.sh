#!/usr/bin/bash

USER=wiggle
GROUP=$USER

case $2 in
    PRE-INSTALL)
	if grep "^$GROUP:" /etc/group > /dev/null 2>&1
	then
	    echo "Group already exists, skipping creation."
	else
	    echo Creating wiggle group ...
	    groupadd $GROUP
	fi
	if id $USER > /dev/null 2>&1
	then
	    echo "User already exists, skipping creation."
	else
	    echo Creating wiggle user ...
	    useradd -g $GROUP -d /var/db/wiggle -s /bin/false $USER
	fi
	echo Creating directories ...
	mkdir -p /var/db/wiggle
	chown -R wiggle:wiggle /var/db/wiggle
	mkdir -p /var/log/wiggle/sasl
	chown -R wiggle:wiggle /var/log/wiggle
	;;
    POST-INSTALL)
	echo Importing service ...
	svccfg import /opt/local/wiggle/etc/wiggle.xml
	echo Trying to guess configuration ...
	IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
	sed --in-place=.bak -e "s/127.0.0.1/${IP}/g" /opt/local/wiggle/etc/vm.args
	sed --in-place=.bak -e "s/127.0.0.1/${IP}/g" /opt/local/wiggle/etc/app.config
	;;
esac
