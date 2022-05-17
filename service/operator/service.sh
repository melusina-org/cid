# service.sh — Service Definitions for admin

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

admin_wizard()
{
    local backupdir
    backupdir="$(tenant_backupdir)"

    if [ ! -d "${backupdir}" ]; then
	install -d "${backupdir}"
    fi
}

# End of file `service.sh'
