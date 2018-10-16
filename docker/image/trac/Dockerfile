### Dockerfile -- Trac System for El Cid

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

COPY ./docker/setup/trac.sh /root/setup/
RUN sh /root/setup/trac.sh

ADD ./docker/image/trac/entrypoint /usr/local/bin/entrypoint

ENTRYPOINT ["/usr/local/bin/entrypoint"]
VOLUME ["/var/git","/var/www","/var/trac"]
