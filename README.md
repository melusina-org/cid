# El Cid, Count of Vivar and Prince of Continuous Integration and Deployment Systems

The **El Cid** project aims at providing a complete continuous
integration and deployment system that is easy to incrementally
improve, to share with team mates and collaborators, and that can be
deployed easily either locally, on bare metal or in the cloud.

It is based on Debian, Ubuntu, Jenkins, Sonartype Nexus and Icinga2.

## Benefits and Features

### Escape the Monolithic Continuous Integration and Deployment Server

Since setting-up and integrating a Jenkins continuous integration
deployment server can be quite a lengthy process, it is common for
organization to start with one server and pack every job related to
all projects into this very server. In a wimplash, this results in a
monolithic system whose availability is criticial to the organization
with a lot of subtle dependencies and conditions that make it hard or
impossible to upgrade.  With **El Cid** a new continuous integration
and development environment can be set up in minutes with a single
command, it makes it very easy to escape the monolithic continuous
integration and deployment server anti-pattern.

### Integrated artefact repositories

After software artefacts have been built and tested in Jenkins, they
must be saved somewhere where systems target of a deployment or an
update can find them.  While the “save produced artefacts” after-build
step can provide a quick expedient to solve this problem, it is not
always well suited for all artefacts, is a source of a security issues
as in this setup the Jenkins server is connected to production
machines, and last this solution lacks all sort of dependency
management features. Therefore **El Cid** integrates software
repositories for common artefact types (DEB, JAR, Tarballs, Docker
Images), supports repository proxying, repository dumps, repostory
restore and repositroy garbage collection.

### Integrate Ansible Playbooks

Because Ansible is so popular as a deployment tool, it is convenient
to integrate it well with the continuous integration and deployment
pipeline. Therefore **El Cid** arranges for starting Jenkins jobs in
Ansible playbooks to be very easy and also arranges for starting
Ansible playbooks in Jenkins to be also easy.  This allows **El Cid**
users to simplify operation privilege delegation by relying on Jenkins
privileges.

### Integrated monitoring system

There is many reasons why monitoring should be part of the continuous
itnegration and deployment pipeline.  Furthermore, because the
monitoring problem has the same input as the deployment problem,
moitoring fits rather naturally in the pipeline. **El Cid** features
Icinga2 and provides useful integrations.

### Integrated backup and restore systen.

### Easy build of Debian packages

### Take your continuous integration and deployment scripts under version control

Continuous integration and deployment scripts are valuable assets of
your organization and they need to be 

XXX

deploy locally, on bare metal as well as in the
cloud.

[![Build Status](https://travis-ci.org/michipili/cid.svg?branch=master)](https://travis-ci.org/michipili/cid?branch=master)

## Setup guide

## Build docker images

~~~ console
% ./tool/docker_build linux admin gitserver jenkins postgresql reverseproxy trac
~~~

## Administrative operations

### Example configuration directory

Here is a listing for a configuration directory to use:

~~~ console
% find ./local -type f
./local/gpg/01234567
./local/cid.conf
./local/ssh/config
./local/ssh/id_rsa_github
./local/Dockerfile
~~~

The `cid.conf` enumerates the trac environments to configure.  Each
environment configured has the form

~~~ conf
[local]
location = /trac/local
admin = alice
~~~

where location stands for the location served by the Apache
server. All other parts are optional.

The SSH configuration is just a regular SSH configuration file which
is made available to the cid user, which allows it to access remote
directories.

The GPG key is added to the keyring of the cid user and can be used to
sign software.

The Dockerfile describe a custom Jenkins installation, with
dependencies tailored to the project.  It can start from `cid/jenkins`.

7
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


### Build special Jenkins image

~~~ console
% ./tool/admin_console -c $(pwd)/Library/Config/local -p local jenkins
~~~


## Deploy with compose

### Up or update

~~~ console
% ./tool/docker_compose -p local up -d
~~~

### Down

~~~ console
% ./tool/docker_compose -p local down
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
