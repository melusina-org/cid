# Dockerfile -- Continuous Delivery Agent

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# Variables:
#
#  GOCD_VERSION (v19.2.0)
#   The version of GoCD to customise.

ARG GOCD_VERSION=v21.4.0
FROM gocd/gocd-agent-ubuntu-20.04:${GOCD_VERSION}

USER root
RUN install -d /opt/cid/libexec/setup
RUN install -o go -g root -d /opt/cid/var/quicklisp

COPY libexec/setup/apt /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/apt

RUN printf 'tzdata\ttzdata/Areas\tselect\tEurope\ntzdata\ttzdata/Zones/Europe\tselect\tParis'\
 | debconf-set-selections

RUN apt install -y clang libffi-dev libgsl-dev libsqlite3-dev texlive texinfo docker.io

COPY libexec/setup/sbcl /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/sbcl

COPY libexec/setup/quicklisp /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/quicklisp -o go -g root\
  alexandria trivia\
  cl-yaml cl-json cxml\
  hunchentoot cl+ssl\
  cl-prevalence

USER go

### End of file `Dockerfile'
