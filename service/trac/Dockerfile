### Dockerfile -- Trac System for El Cid

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

ADD service/trac/entrypoint /opt/cid/bin/entrypoint
ENTRYPOINT ["/opt/cid/bin/entrypoint"]

VOLUME ["/var/git","/var/www","/var/trac"]

### End of file `Dockerfile'
