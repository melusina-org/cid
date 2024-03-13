# docker.sh — Setup Docker

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# Docker group
#
#  In boot2docker, the docker socket is owned by group docker (100)
#  but in Debian, the group 100 is users. We therefore remove the
#  group users and create a group docker with the correct gid.

groupdel users
groupadd -g 100 docker

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 docker-ce

# End of file `docker.sh'
