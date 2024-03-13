# python.sh — Setup python

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
 python3\
 python3-matplotlib\
 python3-pip\
 python3-pymysql\
 sqlite3

env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 nginx-extras

# End of file `python.sh'
