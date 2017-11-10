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
