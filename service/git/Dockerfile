### Dockerfile -- git server

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

ADD ./ /opt/cid/var/src/cid/.
RUN chown -R cid:cid /opt/cid/var/src/cid/.

COPY ./service/admin/subr.sh /root/setup/
RUN sh /root/setup/subr.sh

COPY ./service/admin/admin.sh /root/setup/
RUN sh /root/setup/admin.sh

COPY ./docker/setup/gitserver.sh /root/setup/
RUN sh /root/setup/gitserver.sh

COPY ./service/git/sudoers-git /etc/sudoers.d/git
COPY ./service/git/entrypoint /usr/local/bin/entrypoint

ENTRYPOINT ["/usr/local/bin/entrypoint"]
VOLUME ["/var/git"]
