# El Cid – Count of Vivar and Prince of Continuous Integration and Deployment Systems

The **El Cid** project aims at providing a complete continuous
integration and deployment system that is easy to incrementally
improve, to share with team mates and collaborators, and that can be
deployed easily either locally, on bare metal or in the cloud.


## Benefits and Features

We review the benefits of using **El Cid** and the features of the
system. Since the project is at an early stage, some features are not
yet implemented and are marked with a _[FUTURE]_ tag.

  - Escape the hold of the Monolithic Continuous Integration and
Delivery Server.
  - Instantly recreate the continuous integration and delivery pipepline
after a disaster.
  - [FUTURE] Share your continuous integration and delivery scripts with
your contributors.
  - [FUTURE] Use integrated artefact repositories.
  - [FUTURE] Use integrated monitoring system.

### Escape the hold of the Monolithic Continuous Integration and Delivery Server

Since setting-up and integrating a continuous integration and delivery
server can be quite a lengthy process, it is common for organization
to start with one server and pack every job related to any project
into this very server. In a wimplash, this results in a monolithic
system whose availability is criticial to the organization with a lot
of subtle dependencies and conditions that make it hard or impossible
to upgrade.  With **El Cid** a new continuous integration and delivery
environment can be set up in minutes. It makes it very easy to escape
the monolithic continuous integration and delivery server anti-pattern
by allocating a continuous integration and delivery pipeline per
project.


### Instantly recreate the continuous integration and delivery pipepline after a disaster

Continuous integration and delivery scripts are valuable assets of
your organization and are the results of several improvement
iterations.  They therefore deserve to be kept under version control
and to be dumped in restorable backups.  This is why **El Cid**
implements a dump and restore routine that can rapidly recreate the
corresponding continous integration and delivery pipelines.


### [FUTURE] Share your continuous integration and deployment scripts with your contributors

Projects are accomplished by various contributors, regular employees
of a company, volunteers in a free-software project, externals or
free-lancers hired by a company. These contributors should have
access to continuous integration and delivery pipeline.
**El Cid** makes it easy to share a continuous integration and
delivery environment with contributors without granting them access to
restricted ressoures or secrets simply by allowing us to share our
continuous integration and deployment pipeline as a source code
repository which can instantly be turned into a working continuous
integration and delivery environment.


### [FUTURE] Build Debian packages for your software and simplify operations

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


### [FUTURE] Integrated artefact repositories

After software artefacts have been built and tested in our continuous
integration and delivery pipeline, they must be saved somewhere where
systems target of a deployment or an update can find them.  While the
“save produced artefacts” after-build step can provide a quick
expedient to solve this problem, it is not always well suited for all
artefacts, is a source of a security issues as in this setup the
Jenkins server is connected to production machines, and last this
solution lacks all sort of dependency management features. Therefore
**El Cid** integrates software repositories for common artefact types
(DEB, JAR, Tarballs, Docker Images), supports repository proxying,
repository dumps, repostory restore and repositroy garbage collection.


### [FUTURE] Integrated monitoring system

There is many reasons why monitoring should be part of the continuous
integration and deployment pipeline.  Furthermore, because the
monitoring problem has the same input as the deployment problem,
moitoring fits rather naturally in the pipeline. **El Cid** features
integrates a standard monitoring suite.


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
    On OS X Systems, this is provided by the COLIMA package.

  - A working copy of the master branch of this repository. This can
    be created by exploding [the zip archive created by GitHub][cid-zip]
    or cloning the repository. Ensure that the working copy is in an
    adequate place where QuickLisp can find the Lisp systems defined
    in the working copy.

### Load the El Cid System

~~~ lisp
CL-USER> (ql:quickload '#:org.melusina.cid/user)
CL-USER> (in-package #:org.melusina.cid/user)
~~~

### Create a Colima instance on a Mac

~~~ lisp
CID/USER> (defvar *colima-instance*
            (make-instance 'colima:instance :name "Tourist"))
						   
*COLIMA-INSTANCE*
CID/USER> (colima:start-instance *colima-instance*)
~~~

### Discover the project lifecycle

With a Common Lisp listener, issue the following commands to build
docker image, create, configure, start, dump, restore, stop and delete
a project.

~~~ lisp
CID/USER> (development:build)
CID/USER> (setf operation:*project*
            (operation:make-project :name "tourist"))
CID/USER> (operation:create-project)
CID/USER> (operation:edit-project-configuration-file)
CID/USER> (operation:configure-project)
CID/USER> (operation:start-project)
CID/USER> (operation:dump-project)
CID/USER> (operation:stop-project)
CID/USER> (operation:delete-project)
CID/USER> (operation:create-project)
CID/USER> (operation:configure-project)
CID/USER> (operation:restore-project #p"~/.local/share/org.melusina.cid/local/backups/local.2023-10-31.a.txz")
CID/USER> (operation:start-project)
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

## Administration of GITSERVER

The current server certainly needs to be replaced by a git-tea
instance. However it is still here and the following instructions are
about configuring it and running.

### Authorized Keys

~~~ console
docker exec -it "${PROJECT_NAME}-gitserver-1" vi /var/git/.ssh/authorized_keys
~~~

### Create a Repository

~~~ 
$ docker exec -it "${PROJECT_NAME}-gitserver-1" /usr/local/bin/create_repository TRACNAME REPOSITORYNAME
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
