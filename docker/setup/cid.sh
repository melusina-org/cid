# cid.sh — Setup cid

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

set -ex

opam_user='cid'
opam_group='cid'
opam_root='/opt/opam'
opam_switch='4.04.0'

eval $(opam config env --root=${opam_root})

cd /opt/cid/var/src
(cd ./cid && autoconf)
opam pin add --yes --no-action "cid" "./cid"
opam install --yes cid

# End of file `cid.sh'