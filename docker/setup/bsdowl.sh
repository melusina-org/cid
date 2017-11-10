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
 bmake install
'
