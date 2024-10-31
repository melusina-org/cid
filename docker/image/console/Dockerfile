# Dockerfile — Definition for the Administration Console image

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

COPY ./docker/setup/trac.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/trac.sh

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 cpio\
 clang

ENV PATH="/opt/cid/bin:/opt/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

COPY ./docker/setup/sbcl.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/sbcl.sh

COPY ./docker/setup/quicklisp.sh /opt/cid/var/setup/
RUN PACKAGE=cid sh /opt/cid/var/setup/quicklisp.sh -o cid -g cid

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

USER cid

COPY ./docker/image/console/preload-systems.lisp /opt/cid/var/setup/preload-systems.lisp
RUN sbcl\
 --load /opt/cid/var/quicklisp/setup.lisp\
 --load /opt/cid/var/setup/preload-systems.lisp

ADD --chown=cid:cid ./ /opt/cid/var/quicklisp/local-projects/cid/
RUN sbcl\
 --load /opt/cid/var/quicklisp/setup.lisp\
 --load /opt/cid/var/quicklisp/local-projects/cid/docker/image/console/build-console.lisp\
 --batch

USER root
RUN sbcl\
 --load /opt/cid/var/quicklisp/setup.lisp\
 --load /opt/cid/var/quicklisp/local-projects/cid/docker/image/console/configure-console.lisp\
 --batch
RUN install -m 755 -o root -g root\
 /opt/cid/var/quicklisp/local-projects/cid/console\
 /opt/cid/bin/console

COPY ./docker/image/console/entrypoint /opt/cid/bin/entrypoint
ENTRYPOINT ["/opt/cid/bin/entrypoint"]
#VOLUME ["/opt/cid/share/console"]

# End of file `Dockerfile'
