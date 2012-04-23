This is a web GUI for SmartOS systems, it talks over the CloudAPI, you'll need vmwebadm (https://github.com/Licenser/vmwebadm) to run on your SmartOS. 

It is pretty much alpha ware so don't expect to much!

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

then visit: http://your_host:8080/