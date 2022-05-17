### Dockerfile – operator docker image

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

ARG CID_LINUX_REFERENCE=latest
FROM cid/linux:${CID_LINUX_REFERENCE}

COPY libexec/setup/trac /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/trac

RUN install -d -m 755 -o cid -g cid /opt/cid/var/src/rashell
ADD obj/build/consultancy/cl-rashell.tar /opt/cid/var/src/rashell

ADD --chown=cid:cid ./ /opt/cid/var/src/cid/.

RUN su -l cid -c 'cd /opt/cid/var/src/cid && development/build lisp/cid'\
 && install -m 755 /opt/cid/var/src/cid/obj/lisp/Linux/org.melusina.cid /opt/cid/bin/cid

COPY service/operator/subr.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/subr.sh

COPY service/operator/admin.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/admin.sh

ADD service/operator/entrypoint /opt/cid/bin/entrypoint
ENTRYPOINT ["/opt/cid/bin/entrypoint"]

### End of file `Dockerfile'
