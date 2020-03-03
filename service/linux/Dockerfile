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
# ARG_DEBIAN_MIRROR (none)
#  Debian is distributed (mirrored) on hundreds of servers on the
#  Internet. Using a nearby server will probably speed up your
#  download, and also reduce the load on our central servers and on
#  the Internet as a whole. Example values are
#  ftp.fr.debian.org/debian/ or ftp.de.debian.org/debian/.
#
#  See Also: https://www.debian.org/mirror/list

FROM debian:stretch
ARG DEBIAN_MIRROR=none

RUN install -d -o root -g root -m 700 /root/setup

COPY ./docker/setup/apt.sh /root/setup/
RUN sh -x /root/setup/apt.sh

COPY ./docker/setup/hier.sh /root/setup
RUN env PACKAGE=cid sh -x /root/setup/hier.sh

COPY ./docker/setup/users.sh /root/setup
COPY ./service/linux/users.conf /root/setup/
RUN env PACKAGE=cid sh -x /root/setup/users.sh
