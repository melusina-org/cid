# admin_console — Administration Console Tool

## Synopsis

~~~ console
% ./tool/admin_console -c $(pwd)/project/local create
% ./tool/admin_console -c $(pwd)/project/local configure
% ./tool/admin_console -c $(pwd)/project/local dump
% ./tool/admin_console -c $(pwd)/project/local restore ~/var/backups/example.2018-01-01.a.txz
% ./tool/admin_console -c $(pwd)/project/local rm
% ./tool/admin_console -c $(pwd)/project/local shell
~~~

## Description

This utility can configure an **El Cid** project and perform some
administrative operations, such as dumping and restoring backups.


## Create docker volumes for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local create
~~~

creates docker volume for the project.  Before starting the
application services for the project, the volumes need to be
populated with application data.  Populating the volumes with
application data is achieved either by using the *configure*
subcommand for a new deployment or by using the *restore* subcommand
to recreate the state saved in a previous *dump*.

The precise list of created volumes depends on the list of services
which are enabled in the [project configuration](./cid.conf.md).


## Configure docker volumes for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local configure
~~~

configures docker volume for the project.

The precise list of configured volumes depends on the list of services
which are enabled in the [project configuration](./cid.conf.md).


## Dump application data for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local dump
~~~

dumps application data for the project.  This application data is
saved as a tarball named after the project, the date of the dump and
an alphabetic number like `local.2018-01-01.a.txz`.  The produced
tarball is stored under the path configured under `project.backupdir`
which defaults to the subdirectory `backup` of the configuration
directory.


## Restore application data for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local restore ~/Desktop/local.2018-01-01.a.txz
~~~

restores application data from a previous dump for the project.  A
tarball containing a dump of application data for the project can be
generated with the *dump* subcommand.

Restoring application data for the project is only valid on a project
whose volumes have just been created.


## Remove application data for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local rm
~~~

will remove application data for the project.


## Administrator shell for the project

The command

~~~ console
% ./tool/admin_console -c $(pwd)/project/local shell
~~~

drops to a shell in a container where support programs are available
and application data can be examined.
