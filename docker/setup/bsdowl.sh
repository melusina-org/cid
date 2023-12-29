# bsdowl.sh — Setup bsdowl

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 autoconf\
 bmake\
 libarchive-tools\
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

# End of file `bsdowl.sh'
