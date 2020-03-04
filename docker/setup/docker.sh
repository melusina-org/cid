#
# Docker group
#
#  In boot2docker, the docker socket is owned by group docker (100)
#  but in Debian, the group 100 is users. We therefore remove the
#  group users and create a group docker with the correct gid.

groupdel users
groupadd -g 100 docker

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 docker-ce
