# El Cid, Count of Vivar and Prince of Continuous Integration and Delivery Systems

The **El Cid** project aims at providing a complete continuous
integration and delivery system that is easy to incrementally
improve, to share with team mates and collaborators, and that can be
deployed trivially either locally, on bare metal or in the cloud.

It is based on Debian, Ubuntu, Haproxy, GoCD, Gitea, trac.


# Benefits and Features

- Escape the hold of the Monolithic Continuous Integration and Delivery Server
- Instant recreation of the continuous integration pipepline after a disaster
- Share your continuous integration and deployment scripts with your contributors
- Build Debian packages for your software and simplify operations
- Make your Ansible-based deployment and maintenance routines accessible to your developers
- Integrated artefact repositories
- Integrated monitoring system

See the [benefits and features](./doc/benefits_and_features.md)
discussion for a more in-depth presentation of these topics.


# Quick Initial Setup Guide

*The *quick initial setup* is for users who want to setup and try
rapidly **El Cid** for the first time, with as little effort and
configuration as possible.*

## Acquire and visit a local working copy

~~~ console
% git clone git@github.com:michipili/cid.git
% cd cid
~~~

## Start a tenant shell

The “tenant shell” is a a shell with the `CID_TENANT_DIR` environment
variable set.  It points **cid** tools to configuration files and
assets used to setup and operate the system. Just start with the
“local” tenant which is useful for quick-starting.

~~~ console
% ./tool/tenant_shell local.cid
---> Start a shell for tenant 'local.cid'
~~~

As suggested by the example, it is possible to use reverse DNS
notation to label tenants.

## Build docker images

~~~ console
% ./tool/docker_build
~~~


## Create and initialise environment

This creates docker volumes for persistant data and generates
cryptographic artefacts, a self-signed certificate and a
passphrase-less SSH key to be used by GoCD to retrieve repositories.

~~~ console
% ./tool/admin_console configure
~~~


## Run the environment

We can then run the environment with

~~~ console
% ./tool/docker_compose up
~~~

adjust our local host database with the record

~~~
127.0.0.1	cid.local gocd.cid.local
~~~

and visit our GoCD instance on `gocd.cid.local`.  The tool is a thin
wrapper around docker compose, and the containers can be stopped by
hitting Ctrl-C or using the command `./tool/docker_compose stop`.


## Dump and restore data volumes


### Dump data volumes

The following command dumps docker data volumes for the current
tenant:

~~~ console
% ./tool/admin_console dump
~~~

The result of the dump is a tarball in the configurable backup
directory of the tenant, which is `./local/backups` in our quick setup
environment.  This tarball can be used to restore the environment.

### Restore data volumes

This restores docker data volumes for the current tenant.  Note that
the volumes are dropped and recreated before being restored.

~~~ console
% ./tool/admin_console restore ./backup/local.2018-05-16.a.txz
~~~

### Reclaim data volumes

This destroys data volumes for the current tenant:

~~~ console
% ./tool/admin_console rm
~~~
