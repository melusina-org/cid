# El Cid – Count of Vivar and Prince of Continuous Integration and Deployment Systems

The **El Cid** project aims at providing a complete continuous
integration and deployment system that is easy to incrementally
improve, to share with team mates and collaborators, and that can be
deployed easily either locally, on bare metal or in the cloud.

It is based on Debian, Ubuntu, Jenkins, Sonartype Nexus and Icinga2.


## Benefits and Features

- Escape the hold of the Monolithic Continuous Integration and Deployment Server
- Instant recreation of the continuous integration pipepline after a disaster
- Share your continuous integration and deployment scripts with your contributors
- Build Debian packages for your software and simplify operations
- Make your Ansible-based deployment and maintenance routines accessible to your developers
- Integrated artefact repositories
- Integrated monitoring system

### Escape the hold of the Monolithic Continuous Integration and Deployment Server

Since setting-up and integrating a Jenkins continuous integration
deployment server can be quite a lengthy process, it is common for
organization to start with one server and pack every job related to
all projects into this very server. In a wimplash, this results in a
monolithic system whose availability is criticial to the organization
with a lot of subtle dependencies and conditions that make it hard or
impossible to upgrade.  With **El Cid** a new continuous integration
and development environment can be set up in minutes with a single
command, it makes it very easy to escape the monolithic continuous
integration and deployment server anti-pattern by allocating a
continuous integration and deployment pipeline per project.


### Recreate instantly the continuous integration pipepline after a disaster

Continuous integration and deployment scripts are valuable assets of
your organization and are the results of several improvement
iterations.  They therefore deserve to be kept under version control
or to be dumped in restorable backups.  This is why **El Cid**
implements a dump and restore routine that can rapidly recreate the
corresponding continous integration pipeline.


### Share your continuous integration and deployment scripts with your contributors

Contributors of a project, should they be volunteers in a
free-software project or free-lancers hired by a company, should have
access to your continuous integration and deployment pipeline.
**El Cid** makes it easy to share a continuous integration and
deployment with your contributors without granting them access to
restricted ressoures or secrets simply by allowing you to share your
continuous integration and deployment pipeline as a restorable dump
or as a full VirtualBox appliance which can be deployed instantly even
in UNIX-hostile environments.


### Build Debian packages for your software and simplify operations

Packaging software for the Debian and Ubuntu distributions is a
notoriously complex activity, and there is neither a standard set of
tools to do so, nor an easy case which is is well covered by a
tutorial.  However the creation of software packages has a lot of
benefits, like reproducible tests, reproducible deployments and
dependency management. Therefore **El Cid** provides a Debian package
building tool which is easy to use on software which is easy to build,
support software branches, but does not try to conform to the Debian
packaging guidelines. Therefore the resulting packages are not
suitable for upload on Debian servers but can still be valuable for
internal use in your organisation.


### Make your Ansible-based deployment and maintenance routines accessible to your developers

Because Ansible is so popular as a configuration management and
deployment tool, it is convenient to integrate it well with the
continuous integration and deployment pipeline. Therefore **El Cid**
arranges for starting Jenkins jobs in Ansible playbooks to be very
easy and also arranges for starting Ansible playbooks in Jenkins to be
not less easy.  This allows **El Cid** users to simplify operation
privilege delegation by relying on Jenkins to manage privileges and
secrets required by Ansible playbooks and let software engineers use
one-click deployment buttons. This ability to delegate operation
privileges can be used to remove the dependency of your software team
on the operation team to perform deployments.  It can also support
your implementation of the immutable server pattern and contribute to
the stability of your site, and could also be useful in other
scenarios.


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


### Integrated monitoring system

There is many reasons why monitoring should be part of the continuous
integration and deployment pipeline.  Furthermore, because the
monitoring problem has the same input as the deployment problem,
moitoring fits rather naturally in the pipeline. **El Cid** features
Icinga2 and provides useful integrations with Icinga2 in the
continuous integeration and deployment pipeline.



## Roadmap

  -[ ] Escape the Monolithic Continuous Integration and Deployment Server
  -[ ] Instant recreation of the continuous integration pipepline after a disaster
  -[ ] Share your continuous integration and deployment scripts with your contributors
  -[ ] Integrated artefact repositories
  -[ ] Integrated monitoring system
  -[ ] Simplified build of Debian packages
  -[ ] Make your Ansible Playbooks accessible to your developers


## Quick Initial Setup Guide

*The *quick initial setup* is for users who want to setup and try
rapidly **El Cid** for the first time, with as little effort and
configuration as possible.*

### Prerequisites

Supported operating systems are modern Linux versions and Mac OS X.
This software might also work on BSD Systems featuring a docker
stack. The prerequisites and dependences are:

  - A UNIX system featuring a Lisp implementation, a shell, and basic
    utilities, as described by the last version of POSIX.

  - A docker client configured to interact with an up and running
    docker daemon and the `docker-compose` program. On Linux systens
    the Docker client and daemons are provided by
    [the *docker-ce* package][external-docker-ce] but the
    `docker-compose` program has to be installed separately.
    On OS X Systems, this is provided by
    [the Docker for Mac][external-docker-mac] package.

  - A working copy of the master branch of this repository. This can
    be created by exploding
    [the zip archive created by GitHub][cid-zip] or cloning the
    repository with the following command:

~~~ console
% git clone https://github.com/melusina-org/cid
~~~

### From checkout to up and running

With a Common Lisp listener, issue the following commands:

~~~ lisp
CL-USER> (ql:quickload '#:org.melusina.cid/user)
CL-USER> (in-package #:org.melusina.cid/user)
CID/USER> (development:build)
CID/USER> (operation:create-project)
CID/USER> (operation:configure-project)
CID/USER> (operation:start-project)
~~~

### From checkout to up and running

With the shell, visit a working copy of the **El Cid** and issue the
following command:

~~~ console
% ./tool/wizard_initial_setup my-first-project
~~~

This will start a wizard guiding us through the setup and
initialisation of a project `my-first-project`.  Any name that can be
a UNIX path is allowed.  The wizard starts with a greeting and display
an overview of the initialisation process, then asks for a directory
to hold the configuration of our first project.

~~~ console
Welcome to the initial setup wizard for El Cid!

This program will setup a complete continous integration and
deployment pipeline for your first project.  It will go through the
following steps:

  1. Create initial project configuration.
  2. Build docker images for El Cid.
  3. Create docker volumes for the initial project.
  4. Start El Cid with docker.
  5. Configure Jenkins.
  6. Use the continuous integration and deployment pipeline!


Step 1. Create initial project configuration.

Please choose a directory to hold the configuration of your project [./project/my-first-project]:
~~~

After that the wizard goes on through the next initialisation steps.
Building docker image is subject to a lot of temporary error
conditions and will commonly fail when the host network has failures
or if the online repositories providing 3rd party software are
temporarily unavailable or in an inconsistant state. If such an error
occurs, the wizard will give the chance to restart the failing step or
to abort the initialisation procedure --- in that case, the wizard can
be restarted again at a later time.

~~~ console
Step 2. Build docker images for El Cid.
~~~

Once the docker images for **El Cid** has been built the wizard
creates a few docker volumes, which are persistant data stores for our
containers.  These docker volumes are where Jenkins jobs, software
artefacts and monitoring data are stored.  The wizard then starts the
docker stack for **El Cid**.

~~~ console
Step 3. Create docker volumes for the initial project.
Step 4. Start El Cid with docker.
~~~

~~~
Step 5. Configure Jenkins.
Step 6. Use the continuous integration and deployment pipeline!
~~~


# !!!! The rest of this file is outdated !!!!


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


## Common Operations

### Create data volumes

This creates specific docker data volumes for the `local` project.

~~~ console
% ./tool/admin_console -c $(pwd)/project/local create
~~~


### Configure data volumes

This populates docker data volumes for the `local` project according
to the specification found in the configuration of the `local`
project.

~~~ console
% ./tool/admin_console -c $(pwd)/project/local configure
~~~

### Dump data volumes

This dumps docker data volumes for the `local` project.

~~~ console
% ./tool/admin_console -c $(pwd)/project/local dump
~~~

The result of the dump is a tarball in the backup directory of the
project.  This tarball can be used to restore the environment.

### Restore data volumes

This restores docker data volumes for the `local` project.  Note that
the volumes are dropped and recreated before.

~~~ console
% ./tool/admin_console -c $(pwd)/project/alternate restore ./backup/local.2018-05-16.a.txz
~~~

### Reclaim data volumes

This destroys data volumes for the `local` project.

~~~ console
% ./tool/admin_console -c $(pwd)/project/local rm
~~~


### Build special Jenkins image

~~~ console
% ./tool/admin_console -c $(pwd)/project/local jenkins
~~~


## Deploy with compose

### Up or update

~~~ console
% ./tool/docker_compose -c $(pwd)/project/local up -d
~~~

### Down

~~~ console
% ./tool/docker_compose -c $(pwd)/project/local down
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
  [cid-zip]:            https://github.com/melusina-org/cid
  [external-docker-mac]:https://docs.docker.com/docker-for-mac/install/
  [external-docker-ce]: https://store.docker.com/search?type=edition&offering=community
