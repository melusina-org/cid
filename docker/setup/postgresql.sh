# postgresql.sh — Setup PostgreSQL

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
 postgresql

install -d -o postgres -g postgres -m 750\
 /var/run/postgresql/9.6-main.pg_stat_tmp

# End of file `postgresql.sh'
