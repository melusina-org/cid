### Dockerfile -- Reverse Proxy for El Cid

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

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 haproxy\
 && install -d -o haproxy -g haproxy -m 700 /run/haproxy
ADD service/reverseproxy/haproxy.cfg /etc/haproxy/haproxy.cfg
ADD service/reverseproxy/entrypoint /opt/cid/bin/entrypoint
ENTRYPOINT ["/opt/cid/bin/entrypoint"]
CMD ["haproxy","-db","-f","/etc/haproxy/haproxy.cfg"]

### End of file `Dockerfile'
