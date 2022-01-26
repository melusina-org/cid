### Dockerfile -- Continuous Delivery Agent

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This software is governed by the CeCILL-B license under French law and
# abiding by the rules of distribution of free software.  You can  use,
# modify and/ or redistribute the software under the terms of the CeCILL-B
# license as circulated by CEA, CNRS and INRIA at the following URL
# "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

# Variables:
#
#  GOCD_VERSION (v19.2.0)
#   The version of GoCD to customise.

ARG GOCD_VERSION=v19.2.0
FROM gocd/gocd-agent-ubuntu-18.04:${GOCD_VERSION}

RUN mkdir -p /opt/cid/libexec/setup

COPY libexec/setup/apt /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/apt

COPY libexec/setup/sbcl /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/sbcl

COPY libexec/setup/quicklisp /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/quicklisp -o go -g go\
  alexandria trivia\
  cl-yaml cl-json cxml\
  hunchentoot cl+ssl\
  cl-prevalence

### End of file `Dockerfile'
