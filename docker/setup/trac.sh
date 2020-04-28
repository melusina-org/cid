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


env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 apache2\
 git\
 git-core\
 libapache2-mod-wsgi\
 openssh-server\
 pwgen\
 python-flup\
 python-pip\
 python-psycopg2\
 python-pygments\
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

a2enmod wsgi

install -d -o www-data -g www-data -m 750\
 /var/trac

ln -s /var/trac/sites /etc/apache2/sites-trac

cat > /etc/apache2/ports.conf <<PORTS-CONF
# We only listen to 80, as SSL termination is implemented by
# the reverseproxy.
Listen 80
PORTS-CONF

sed -i -e '
/IncludeOptional sites-enabled[/][*][.]conf/a\
IncludeOptional sites-trac/*.conf

/<Directory [/]srv[/]>/i\
<Directory /var/trac/www/>\
        Options Indexes FollowSymLinks\
        AllowOverride None\
        Require all granted\
</Directory>\
' /etc/apache2/apache2.conf

cat >> /etc/apache2/apache2.conf <<APACHE

<Location "/server-status">
    SetHandler server-status
    Require all granted
</Location>
APACHE
