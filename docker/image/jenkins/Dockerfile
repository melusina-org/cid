# Dockerfile — Dockerfile for Jenkins

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

ARG CID_LINUX_REFERENCE=latest
FROM cid/linux:${CID_LINUX_REFERENCE}

COPY ./docker/setup/jenkins.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/jenkins.sh

COPY ./docker/setup/docker.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/docker.sh

COPY ./docker/setup/sbcl.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/sbcl.sh

COPY ./docker/setup/java.sh /opt/cid/var/setup/
RUN sh /opt/cid/var/setup/java.sh

COPY ./docker/setup/quicklisp.sh /opt/cid/var/setup/
RUN env PACKAGE=cid sh /opt/cid/var/setup/quicklisp.sh -o jenkins -g jenkins -i

RUN usermod -a -G docker jenkins

ENV JENKINS_HOME "/var/lib/jenkins"

ADD ./docker/image/jenkins/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]

# End of file `Dockerfile'
