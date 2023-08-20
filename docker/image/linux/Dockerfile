### Dockerfile -- Custom Linux System

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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
COPY ./docker/image/linux/users.conf /root/setup/
RUN env PACKAGE=cid sh -x /root/setup/users.sh

# End of file