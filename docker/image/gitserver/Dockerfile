### Dockerfile -- git server

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

FROM cid/linux

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 cpio

COPY ./docker/setup/trac.sh /root/setup/
RUN sh /root/setup/trac.sh

COPY ./docker/setup/gitserver.sh /root/setup/
RUN sh /root/setup/gitserver.sh

COPY ./docker/setup/bsdowl.sh /root/setup/
RUN sh /root/setup/bsdowl.sh

COPY ./docker/setup/docker.sh /root/setup/
RUN sh /root/setup/docker.sh

COPY ./docker/image/admin/subr.sh /root/setup/
ADD ./subr /opt/cid/var/src/cid/subr
RUN chown -R cid:cid /opt/cid/var/src/cid/subr
RUN sh /root/setup/subr.sh

COPY ./docker/image/admin/support.sh /root/setup/
ADD ./support /opt/cid/var/src/cid/support
RUN chown -R cid:cid /opt/cid/var/src/cid/support
RUN sh /root/setup/support.sh

COPY ./docker/image/gitserver/sudoers-gitserver /etc/sudoers.d/gitserver
COPY ./docker/image/gitserver/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]
VOLUME ["/var/git"]
