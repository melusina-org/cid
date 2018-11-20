env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 autoconf\
 bmake\
 curl\
 default-jdk\
 gawk\
 git\
 gnupg\
 jenkins\
 m4\
 ssh\
 pkg-config

sed -i -e '
/JENKINS_ARGS/{
 i\
JENKINS_ARGS="--webroot=/var/cache/$NAME/war --httpPort=$HTTP_PORT --prefix=/$NAME"
 d
}
' /etc/default/jenkins
