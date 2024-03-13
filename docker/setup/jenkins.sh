# jenkins.sh — Setup Jenkins

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 ansible\
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

# End of file `jenkins.sh'
