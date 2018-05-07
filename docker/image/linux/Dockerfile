### Dockerfile -- Custom Linux System

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid
#
# Copyright © 2018 Michaël Le Barbier
#
# This file must be used under the terms of the MIT license.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at
# https://opensource.org/licenses/MIT

FROM debian:stretch

RUN install -d -o root -g root -m 700 /root/setup

COPY ./docker/setup/apt.sh /root/setup/
RUN sh -x /root/setup/apt.sh

COPY ./docker/setup/hier.sh /root/setup
RUN env PACKAGE=cid sh -x /root/setup/hier.sh

COPY ./docker/setup/users.sh /root/setup
COPY ./docker/image/linux/users.conf /root/setup/
RUN env PACKAGE=cid sh -x /root/setup/users.sh
