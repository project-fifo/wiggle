#!/usr/bin/bash

USER=wiggle
GROUP=www
DOMAIN="project-fifo.net"
CERTDIR="/var/db/fifo"
SUBJ="
C=AU
ST=Victoria
O=Company
localityName=Melbourne
commonName=$DOMAIN
organizationalUnitName=Widgets
emailAddress=blah@blah.com
"



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
        chown -R $USER:$GROUP /var/db/wiggle
        mkdir -p /var/log/wiggle/sasl
        chown -R $USER:$GROUP /var/log/wiggle
        if [ ! -d /var/db/fifo ]
        then
            echo "Creating certificates"
            mkdir -p $CERTDIR
            chgrp -R $GROUP $CERTDIR

            openssl genrsa -des3 -out $CERTDIR/$DOMAIN.key -passout env:PASSPHRASE 2048
            fail_if_error $?

            openssl req \
                -new \
                -batch \
                -subj "$(echo -n "$subj" | tr "\n" "/")" \
                -key $CERTDIR/$DOMAIN.key \
                -out $CERTDIR/$DOMAIN.csr \
                -passin env:PASSPHRASE

            cp $CERTDIR/$DOMAIN.key $CERTDIR/$DOMAIN.key.org

            openssl rsa -in $CERTDIR/$DOMAIN.key.org -out $CERTDIR/$DOMAIN.key -passin env:PASSPHRASE

            openssl x509 -req -days 365 -in $CERTDIR/$DOMAIN.csr -signkey $CERTDIR/$DOMAIN.key -out $CERTDIR/$DOMAIN.crt
        fi


        ;;
    POST-INSTALL)
        if svcs svc:/network/wiggle:default > /dev/null 2>&1
        then
            echo Service already existings ...
        else
            echo Importing service ...
            svccfg import /opt/local/wiggle/etc/wiggle.xml
        fi
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
        if [ ! -f /opt/local/wiggle/etc/vm.args ]
        then
            cp /opt/local/wiggle/etc/vm.args.example /opt/local/wiggle/etc/vm.args
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/wiggle/etc/vm.args
        fi
        if [ ! -f /opt/local/wiggle/etc/app.config ]
        then
            cp /opt/local/wiggle/etc/app.config.example /opt/local/wiggle/etc/app.config
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/wiggle/etc/app.config
        fi
        ;;
esac
