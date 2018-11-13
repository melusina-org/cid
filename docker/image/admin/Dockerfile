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
 apache2\
 git\
 git-core\
 libapache2-mod-wsgi\
 openssh-server\
 pwgen\
 python-flup\
 python-pip\
 python-psycopg2\
 subversion\
 trac-accountmanager\
 trac-authopenid\
 trac-bitten\
 trac-customfieldadmin\
 trac-diavisview\
 trac-graphviz\
 trac-mastertickets\
 trac-tags\
 trac-wysiwyg\
 trac-xmlrpc\
 trac

RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 cpio

ENV PATH="/opt/cid/bin:/opt/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

COPY ./docker/setup/sbcl.sh /root/setup/
RUN sh /root/setup/sbcl.sh

COPY ./docker/setup/quicklisp.sh /root/setup/
RUN sh /root/setup/quicklisp.sh -o cid -g cid

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
