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

opam_pin_add gasoline https://github.com/michipili/gasoline
opam_pin_add lemonade-sqlite https://github.com/michipili/lemonade-sqlite
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
