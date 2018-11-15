### Dockerfile -- Reverse Proxy for El Cid

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

ARG CID_LINUX_REFERENCE=latest
FROM cid/linux:${CID_LINUX_REFERENCE}

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 haproxy\
 && install -d -o haproxy -g haproxy -m 700 /run/haproxy
ADD ./docker/image/reverseproxy/haproxy.cfg /etc/haproxy/haproxy.cfg
ADD ./docker/image/reverseproxy/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]
CMD ["haproxy","-db","-f","/etc/haproxy/haproxy.cfg"]
