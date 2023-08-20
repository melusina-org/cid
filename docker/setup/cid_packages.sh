# cid_packages.sh — Setup packages

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 aspcud\
 autoconf\
 awscli\
 bmake\
 cpio\
 curl\
 docker-engine\
 gawk\
 git\
 gnupg\
 graphicsmagick\
 default-jre\
 libbz2-dev\
 libyaml-dev\
 libsqlite3-dev\
 libssl-dev\
 m4\
 nodejs\
 ocaml\
 opam\
 opensp\
 pkg-config\
 python-dev\
 python-pip\
 sgml-data\
 sqlite3\
 tidy

# End of file `cid_packages.sh'
