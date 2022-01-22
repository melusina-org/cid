### apt.sh -- Setup apt

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
  apt-transport-https\
  apt-utils\
  bzip2\
  ca-certificates\
  curl\
  debconf-utils\
  git\
  less\
  lsb-release\
  lsof\
  man\
  net-tools\
  nvi\
  procps\
  ruby-mustache\
  sudo\
  wget\
  xz-utils

apt-get update -y
apt-get upgrade -y
apt-get clean

### End of file `apt.sh'
