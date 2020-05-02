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
 openssh-server\
 subversion\
 git

install -d /run/sshd

sed -i -E -e '
/PubkeyAuthentication (yes|no)/{
 i\
PubkeyAuthentication yes
 d
}

/PasswordAuthentication (yes|no)/{
 i\
PasswordAuthentication no
 d
}
' /etc/ssh/sshd_config


chsh -s /usr/bin/git-shell\
 git

install -d -o git -g git -m 750 /var/git
install -d -o git -g git -m 700 /var/git/.ssh
install -o git -g git -m 600 /dev/null /var/git/.ssh/authorized_keys
