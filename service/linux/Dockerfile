### Dockerfile -- Custom Linux System

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


# Arguments:
#
# PACKAGE (cid)
#  The Unix safe name for the package on the docker image.
#  This name is used in pathnames for various resource, to isolate
#  them from the operating system and third-party software.
#
# DEBIAN_MIRROR (none)
#  Debian is distributed (mirrored) on hundreds of servers on the
#  Internet. Using a nearby server will probably speed up your
#  download, and also reduce the load on our central servers and on
#  the Internet as a whole. Example values are
#  ftp.fr.debian.org/debian/ or ftp.de.debian.org/debian/.
#
#  See Also: https://www.debian.org/mirror/list

FROM debian:stretch
ARG DEBIAN_MIRROR=none
ARG PACKAGE=cid

RUN install -d -o root -g root -m 700\
 /opt/${PACKAGE}/etc/setup\
 /opt/${PACKAGE}/etc/verify\
 /opt/${PACKAGE}/libexec/setup\
 /opt/${PACKAGE}/libexec/verify

COPY libexec/setup/apt /opt/${PACKAGE}/libexec/setup/apt
RUN /opt/${PACKAGE}/libexec/setup/apt

COPY libexec/setup/hier /opt/${PACKAGE}/libexec/setup/hier
RUN env PACKAGE=${PACKAGE} /opt/${PACKAGE}/libexec/setup/hier

COPY libexec/setup/users /opt/${PACKAGE}/libexec/setup/users
COPY service/linux/users.conf /opt/${PACKAGE}/etc/setup/users.conf
RUN env PACKAGE=${PACKAGE} /opt/${PACKAGE}/libexec/setup/users

COPY libexec/setup/sbcl /opt/${PACKAGE}/libexec/setup/sbcl
RUN /opt/${PACKAGE}/libexec/setup/sbcl

COPY libexec/setup/quicklisp /opt/${PACKAGE}/libexec/setup/quicklisp
RUN /opt/${PACKAGE}/libexec/setup/quicklisp -o ${PACKAGE} -g ${PACKAGE}

COPY libexec/setup/bsdowl /opt/cid/libexec/setup/bsdowl
RUN /opt/cid/libexec/setup/bsdowl

COPY libexec/setup/testimage /opt/${PACKAGE}/libexec/setup/testimage
COPY service/linux/testimage /opt/${PACKAGE}/libexec/testimage/linux

ENV PATH="/opt/${PACKAGE}/sbin:/opt/${PACKAGE}/bin:/opt/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

### End of file `Dockerfile'
