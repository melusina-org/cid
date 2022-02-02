### admin.sh -- Install admin programs in a docker image

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


set -e

su -l cid -c '
 set -e
 cd /opt/cid/var/src/cid/admin
 autoconf
 ./configure --prefix=/opt/cid
 bmake -I/usr/local/share/bsdowl all
'

( cd /opt/cid/var/src/cid/admin && bmake -I/usr/local/share/bsdowl install )

cp -R /opt/cid/var/src/cid/service  /opt/cid/share/service

### End of file `admin.sh'
