env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 autoconf\
 bmake\
 curl\
 gawk\
 git\
 gnupg\
 jenkins\
 m4\
 pkg-config

sed -i -e '
/JENKINS_ARGS/{
 i\
JENKINS_ARGS="--webroot=/var/cache/$NAME/war --httpPort=$HTTP_PORT --prefix=/$NAME"
 d
}
' /etc/default/jenkins
