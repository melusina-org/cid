# Dockerfile -- gitserver

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

ADD ./ /opt/cid/var/src/cid/.
RUN chown -R cid:cid /opt/cid/var/src/cid/.

COPY service/operator/subr.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/subr.sh

COPY service/operator/admin.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/admin.sh

COPY libexec/setup/gitserver /opt/cid/libexec/setup/
RUN /opt/cid/libexec/setup/gitserver

COPY service/gitserver/sudoers-git /etc/sudoers.d/git
COPY service/gitserver/entrypoint /opt/cid/bin/entrypoint

ENTRYPOINT ["/opt/cid/bin/entrypoint"]
VOLUME ["/var/git"]

# End of file `Dockerfile'
