# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid
#
# Copyright © 2018 Michaël Le Barbier
#
# This file must be used under the terms of the MIT license.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at
# https://opensource.org/licenses/MIT

FROM cid/linux

COPY ./docker/setup/jenkins.sh /root/setup/
RUN sh /root/setup/jenkins.sh

COPY ./docker/setup/docker.sh /root/setup/
RUN sh /root/setup/docker.sh

COPY ./docker/setup/sbcl.sh /root/setup/
RUN sh /root/setup/sbcl.sh

COPY ./docker/setup/quicklisp.sh /root/setup/
RUN sh /root/setup/quicklisp.sh -o jenkins -g jenkins

RUN usermod -a -G docker jenkins

ENV JENKINS_HOME "/var/lib/jenkins"

ADD ./docker/image/jenkins/sbclrc /var/lib/jenkins/.sbclrc
ADD ./docker/image/jenkins/entrypoint /usr/local/bin/entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint"]

VOLUME ["/var/lib/jenkins"]
