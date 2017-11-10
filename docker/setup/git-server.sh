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
