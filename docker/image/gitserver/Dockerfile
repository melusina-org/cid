# Dockerfile — git server

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

ARG CID_LINUX_REFERENCE=latest
FROM cid/linux:${CID_LINUX_REFERENCE}

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 cpio

COPY ./docker/setup/trac.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/trac.sh

COPY ./docker/setup/gitserver.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/gitserver.sh

COPY ./docker/setup/bsdowl.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/bsdowl.sh

COPY ./docker/image/console/subr.sh /opt/cid/var/setup/
ADD ./subr /opt/cid/var/src/cid/subr
RUN chown -R cid:cid /opt/cid/var/src/cid/subr
RUN sh /opt/cid/var/setup/subr.sh

COPY ./docker/image/console/support.sh /opt/cid/var/setup/
ADD ./support /opt/cid/var/src/cid/support
RUN chown -R cid:cid /opt/cid/var/src/cid/support
RUN sh /opt/cid/var/setup/support.sh

COPY ./docker/image/gitserver/sudoers-gitserver /etc/sudoers.d/gitserver
COPY ./docker/image/gitserver/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]
VOLUME ["/var/git"]

# End of file `Dockerfile'
