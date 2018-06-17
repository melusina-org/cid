# El Cid, Count of Bivar and Prince of Continuous Integration and Deployment Systems

The **El Cid** project aims at providing a continuous integration and
deployment system that is easy to deploy locally as well as in the
cloud.

[![Build Status](https://travis-ci.org/michipili/cid.svg?branch=master)](https://travis-ci.org/michipili/cid?branch=master)

## Setup guide

## Build docker images

~~~ console
% ./tool/docker_build linux admin gitserver jenkins postgresql reverseproxy trac
~~~

## Configure a Project


## Administrative operations

### Example configuration directory

The SSH configuration is just a regular SSH configuration file which
is made available to the cid user, which allows it to access remote
directories.

The GPG key is added to the keyring of the cid user and can be used to
sign software.

~~~ console
% find ./local -type f
./local/gpg/01234567
./local/cid.conf
./local/ssh/config
./local/ssh/id_rsa_github
~~~

The `cid.conf` enumerates the trac environments to configure.  Each
environment configured has the form

~~~ conf
[local]
location = /trac/local
admin = alice
~~~

where location stands for the location served by the Apache server.

### Create data volumes

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local create
~~~


### Configure data volumes

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local configure
~~~

### Dump data volumes

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local dump
~~~

### Restore data volumes

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local restore ./backup/local.2018-05-16.a.txz
~~~

### Reclaim data volumes

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local rm
~~~


## Free software

El Cid is free software: copying it and redistributing it is very
much welcome under conditions of the [MIT][licence-url] licence
agreement, found in the [LICENSE][licence-file] file of the
distribution.

Michaël Le Barbier in Bonn, on Mai 26, 2015

  [licence-url]:        https://opensource.org/licenses/MIT
  [licence-file]:       LICENSE
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
