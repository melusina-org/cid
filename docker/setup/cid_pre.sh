# cid_pre.sh — Setup CID prelimiaries

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

: ${opam_root:=/opt/opam}

opam_pin_add()
(
    set -e
    cd /opt/cid/var/src
    git clone "$2" "$1"
    (cd "$1" && autoconf)
    opam pin add --yes --no-action "$1" "$1"
)

eval $(opam config env --root=${opam_root})

opam_pin_add gasoline https://github.com/foretspaisibles/gasoline
opam_pin_add lemonade-sqlite https://github.com/foretspasibles/lemonade-sqlite
opam install --yes\
  broken\
  bsdowl\
  lemonade\
  gasoline\
  lemonade-sqlite\
  ocamlfind\
  cohttp-lwt\
  ppx_deriving_yojson\
  ssl

opam install --yes\
  webmachine

# End of file `cid_pre.sh'
