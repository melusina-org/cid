# apt.sh — Setup apt

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# Arguments:
#
# DEBIAN_MIRROR (none)
#  Debian is distributed (mirrored) on hundreds of servers on the
#  Internet. Using a nearby server will probably speed up your
#  download, and also reduce the load on our central servers and on
#  the Internet as a whole. Example values are
#  ftp.fr.debian.org/debian/ or ftp.de.debian.org/debian/.
#
#  See Also: https://www.debian.org/mirror/list

: ${DEBIAN_MIRROR:=none}

DEBIAN_FRONTEND=noninteractive
export DEBIAN_FRONTEND


if [ "${DEBIAN_MIRROR}" = 'none' ]; then
    : 'Do not use a specific mirror'
else
# At the time of writing, there is a problem with Debian CDN.
sed -e "s|@DEBIAN_MIRROR@|${DEBIAN_MIRROR}|g" > /etc/apt/sources.list <<APT-CONF
deb http://@DEBIAN_MIRROR@ stretch main
deb http://@DEBIAN_MIRROR@ stretch-updates main
deb http://security.debian.org/debian-security stretch/updates main
APT-CONF
fi

apt-get update -y
apt-get upgrade -y
apt-get install -y pinentry-curses
apt-get install -y gnupg2


# Install basic packages
#
#  Some utilities, like apt-* and debconf-* are useful for package
#  management, the less pager is very useful, nvi is an old version
#  of vi (355 kB including documentation!) which is useful for basic
#  editing and curl is useful as a general debugging and download
#  tool.
#
#  We also install procps (for ps(1)), curl and netcast as they belong
#  to the docker container toolkit.
#
#  Last wget and lsb-release are dependencies of the mysql-apt-config
#  package used to add repositories needed for mysql.

printf 'APT::Install-Recommends "false";\n'\
       >> /etc/apt/apt.conf.d/90norecommends

apt-get remove cmdtest

apt-get install -y\
  autoconf\
  apt-transport-https\
  apt-utils\
  bmake\
  bzip2\
  ca-certificates\
  curl\
  debconf-utils\
  git\
  less\
  lsb-release\
  lsof\
  man\
  libarchive-tools\
  net-tools\
  nvi\
  procps\
  ruby-mustache\
  sudo\
  wget\
  xz-utils


cat >> /etc/apt/sources.list <<EOF
deb http://deb.debian.org/debian bookworm-backports main contrib non-free
EOF

apt-get update -y
apt-get upgrade -y
apt-get clean

# End of file `apt.sh'
