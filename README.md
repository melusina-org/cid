# El Cid, Count of Vivar and Prince of Continuous Integration and Delivery Systems

The **El Cid** project aims at providing a complete continuous
integration and delivery system that is easy to incrementally
improve, to share with team mates and collaborators, and that can be
deployed trivially either locally, on bare metal or in the cloud.

It is based on Debian, Ubuntu, Haproxy, Jenkins, GoCD, Gitea, trac.


# Benefits and Features

- Escape the hold of the Monolithic Continuous Integration and Deployment Server
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

~~~ console
% ./tool/tenant_shell local.cid
./tool/tenant_shell local.cid
---> Start a shell for tenant 'local.cid'
~~~

## Build docker images

~~~ console
% ./tool/docker_build
~~~
