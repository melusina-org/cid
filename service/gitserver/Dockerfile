### Dockerfile -- gitserver

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

### End of file `Dockerfile'
