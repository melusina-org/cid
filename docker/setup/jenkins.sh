sed -i -e '
/JENKINS_ARGS/{
 i\
JENKINS_ARGS="--webroot=/var/cache/$NAME/war --httpPort=$HTTP_PORT --prefix=/$NAME"
 d
}
' /etc/default/jenkins
