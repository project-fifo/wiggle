#!/usr/bin/bash

USER=wiggle
GROUP=wiggle

case $2 in
    PRE-INSTALL)
        if grep '^Image: base64 13.[234].*$' /etc/product
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
        svccfg import /opt/local/fifo-wiggle/share/wiggle.xml
        CONFFILE=/opt/local/fifo-wiggle/etc/wiggle.conf

        if [ ! -f "${CONFFILE}" ]
        then
            cp ${CONFFILE}.example ${CONFFILE}
            sed --in-place -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
            md5sum ${CONFFILE} > ${CONFFILE}.md5
        elif [ -f ${CONFFILE}.md5 ]
        then
            if md5sum --quiet --strict -c ${CONFFILE}.md5 2&> /dev/null
            then
                echo "The config was not adjusted we'll regenerate it."
                cp ${CONFFILE}.example ${CONFFILE}
                sed --in-place -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
                md5sum ${CONFFILE} > ${CONFFILE}.md5
            fi
        fi
        ;;
esac
