This is a web GUI for SmartOS systems, it talks over the CloudAPI, you'll need vmwebadm (https://github.com/project-fifo/vmwebadm) to run on your SmartOS. 

It is pretty much alpha ware so don't expect to much!


Thanks
======
This is long overdue - many thanks to:
* the folks from Joyent - for making this possible.
* #smartos - for answering tons of questions
* and especially Trenster - for good ideas and discussions, tons of testing, and lots of good PR

Features
========

This reflects to so far implemented features not all possible ones.

* machines
  * listing
  * viewing details
  * starting/stopping/restarting
* listing packages
* listing datasets
* creating users
* Adjusting CloudAPI Credentials (name, key, key_id)


Screenshots
===========

* Home view: http://s7.directupload.net/file/d/2869/76xkwhil_png.htm
* Admin view: http://s7.directupload.net/file/d/2869/woghq83k_png.htm
* Account view: http://s1.directupload.net/file/d/2869/kv3neins_png.htm

Installation
============

* Create a smartOS64 zone.
* Grab the tarball from the download section.
* Extract the tarball to /fifo
* run: ```svcadm import /fifo/libs/wigge-*/priv/wiggle.xml


Building
========

Requirements
------------
The following things are required: 

* erlang r15(b01).
* build environment (gcc/make and freinds).

Configuring
-----------

edit the standalone.config to reflect your CloudAPI Host.

Making the thing
----------------

```
./rebar get-deps
make
```

Running
-------

```
make shell
```

```
wiggle:start().
```

then visit: http://your_host:8080/ you can log in with admin / admin.
