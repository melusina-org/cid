# java.sh — Setup Java

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
 maven\
 openjdk-8-jdk\
 openjdk-8-doc

# End of file `java.sh'
