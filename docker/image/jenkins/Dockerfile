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

COPY ./docker/setup/jenkins.sh /root/setup/
RUN sh /root/setup/jenkins.sh

COPY ./docker/setup/docker.sh /root/setup/
RUN sh /root/setup/docker.sh

COPY ./docker/setup/sbcl.sh /root/setup/
RUN sh /root/setup/sbcl.sh

COPY ./docker/setup/java.sh /root/setup/
RUN sh /root/setup/java.sh

COPY ./docker/setup/quicklisp.sh /root/setup/
RUN env PACKAGE=cid sh /root/setup/quicklisp.sh -o jenkins -g jenkins -i

RUN usermod -a -G docker jenkins

ENV JENKINS_HOME "/var/lib/jenkins"

ADD ./docker/image/jenkins/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]

# End of file `Dockerfile'
