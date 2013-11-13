#!/usr/bin/bash

USER=wiggle
GROUP=wiggle

case $2 in
    PRE-INSTALL)
        if grep '^Image: base64 13.2.*$' /etc/product
        then
            echo "Image version supported"
        else
            echo "This image version is not supported please use the base64 13.2.1 image."
            exit 1
        fi
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
        chown -R $USER:$GROUP /var/db/wiggle
        mkdir -p /var/log/wiggle/sasl
        chown -R $USER:$GROUP /var/log/wiggle
        ;;
    POST-INSTALL)
        if svcs svc:/network/wiggle:default > /dev/null 2>&1
        then
            echo Service already existings ...
        else
            echo Importing service ...
            svccfg import /opt/local/fifo-wiggle/share/wiggle.xml
        fi
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
        if [ ! -f /opt/local/fifo-wiggle/etc/vm.args ]
        then
            cp /opt/local/fifo-wiggle/etc/vm.args.example /opt/local/fifo-wiggle/etc/vm.args
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/fifo-wiggle/etc/vm.args
        fi
        if [ ! -f /opt/local/fifo-wiggle/etc/app.config ]
        then
            cp /opt/local/fifo-wiggle/etc/app.config.example /opt/local/fifo-wiggle/etc/app.config
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/fifo-wiggle/etc/app.config
        fi
        ;;
esac
