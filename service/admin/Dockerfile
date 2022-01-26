### Dockerfile – admin docker image

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

ADD ./ /opt/cid/var/src/cid/.
RUN chown -R cid:cid /opt/cid/var/src/.

RUN su -l cid -c 'cd /opt/cid/var/src/cid && development/build lisp/cid'\
 && install -m 755 /opt/cid/var/src/cid/obj/lisp/Linux/org.melusina.cid /opt/cid/bin/cid

COPY service/admin/subr.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/subr.sh

COPY service/admin/admin.sh /opt/cid/libexec/setup/
RUN sh /opt/cid/libexec/setup/admin.sh

### End of file `Dockerfile'
