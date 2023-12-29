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

  - [ ] Escape the Monolithic Continuous Integration and Deployment Server
  - [ ] Instant recreation of the continuous integration pipepline after a disaster
  - [ ] Share your continuous integration and deployment scripts with your contributors
  - [ ] Integrated artefact repositories
  - [ ] Integrated monitoring system
  - [ ] Simplified build of Debian packages
  - [ ] Make your Ansible Playbooks accessible to your developers


## Quick Initial Setup Guide

*The *quick initial setup* is for users who want to setup and try
rapidly **El Cid** for the first time, with as little effort and
configuration as possible.*

### Prerequisites

Supported operating systems are modern Linux versions and Mac OS X.
This software might also work on BSD Systems featuring a docker
stack. The prerequisites and dependences are:

  - A UNIX system featuring a Lisp implementation, QuickLisp, a shell,
    and basic utilities, as described by the last version of POSIX.
	
  - A docker client configured to interact with an up and running
    docker daemon and the `docker-compose` program. On Linux systens
    the Docker client and daemons are provided by
    [the *docker-ce* package][external-docker-ce] but the
    `docker-compose` program has to be installed separately.
    On OS X Systems, this is provided by
    [the Docker for Mac][external-docker-mac] package.

  - A working copy of the master branch of this repository. This can
    be created by exploding [the zip archive created by GitHub][cid-zip]
    or cloning the repository. Ensure that the working copy is in an
    adequate place where QuickLisp can find the Lisp systems defined
    in the working copy.

### Discover the project lifecycle

With a Common Lisp listener, issue the following commands to build
docker image, create, configure, start, dump, restore, stop and delete
a project.

~~~ lisp
CL-USER> (ql:quickload '#:org.melusina.cid/user)
CL-USER> (in-package #:org.melusina.cid/user)
CID/USER> (development:build)
CID/USER> (operation:create-project)
CID/USER> (operation:configure-project)
CID/USER> (operation:start-project)
CID/USER> (operation:dump-project)
CID/USER> (operation:restore-project #p"~/.local/share/org.melusina.cid/local/backups/local.2023-10-31.a.txz")
CID/USER> (operation:stop-project)
CID/USER> (operation:delete-project)
~~~

The commands in the `org.melusina.cid/operation` system operate on the
`org.melusina.cid/operation:*project*` which is setup with defaults
allowing small experiments.

### Run the testsuite

From the command line, use the following command:

~~~ console
development/testsuite
~~~

Alernatively, with a Common Lisp listener, issue the following
commands:

~~~ lisp
CL-USER> (ql:quickload '#:org.melusina.cid/testsuite)
CL-USER> (org.melusina.cid/testsuite:run-all-tests)
~~~


## Common Operations

### Create a project

This creates the `*local*` project:

~~~ lisp
CID/USER> (defparameter *local* (operation:make-project :name "local"))
~~~

See the `org.melusina.cid/operation:make-project` documentation for
additional parameters that can be set when creating the project.

### Create data volumes

This creates specific docker data volumes for the `*local*` project.

~~~ lisp
CID/USER> (operation:create-project *local*)
~~~

### Configure data volumes

This populates docker data volumes for the `*local*` project according
to the specification found in the configuration of the `*local*`
project.

~~~ lisp
CID/USER> (operation:configure-project *local*)
~~~

### Dump data volumes

This dumps docker data volumes for the `*local*` project.

~~~ lisp
CID/USER> (operation:dump-project *local*)
~~~

The result of the dump is a tarball in the backup directory of the
project.  This tarball can be used to restore the environment.

### Restore data volumes

This restores docker data volumes for the `*local*` project.  Note that
the volumes are dropped and recreated before.

~~~ lisp
CID/USER> (operation:restore-project #p"./backup/local.2018-05-16.a.txz" *local*)
~~~

### Reclaim data volumes

This destroys data volumes for the `*local*` project.

~~~ lisp
CID/USER> (operation:delete-project *local*)
~~~

### Start project with docker compose

~~~ lisp
CID/USER> (operation:start-project *local*)
~~~

### Stop project with docker compose

~~~ lisp
CID/USER> (operation:stop-project *local*)
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
