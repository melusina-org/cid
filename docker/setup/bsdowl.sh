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
 autoconf\
 bmake\
 bsdtar\
 curl

install -d -o cid -g cid -m 700 /usr/local/src
su -l cid -c '
 set -e
 cd /usr/local/src
 curl -L https://github.com/michipili/bsdowl/archive/master.zip | bsdtar xf -
 cd bsdowl-master
 autoconf
 ./configure --prefix=/usr/local --with-credentials=sudo
 bmake all
'

( cd /usr/local/src/bsdowl-master && bmake install )
