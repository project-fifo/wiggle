This is a web GUI for SmartOS systems, it talks over the CloudAPI, you'll need vmwebadm (https://github.com/Licenser/vmwebadm) to run on your SmartOS. 

It is pretty much alpha ware so don't expect to much!

Building
========

Requirements
------------
The following things are required: 

* erlang r15(b01).
* build environment (gcc/make and freinds).

Making the thing
----------------

```
./rebar get-deps
make shell
```

Configuring
-----------

edit the standalone.config to reflect your CloudAPI Host.


Running
-------

```
make shell
```


```
wiggle:start().
```


then visit: http://<your host>:8080/