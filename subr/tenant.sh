# tenant.sh — Functions for tenants

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

tenant_name()
{
    if config 'tenant.name'; then
	:
    else
	printf 'local.cid'
    fi
}


tenant_backupdir()
{
    if config 'tenant.backupdir'; then
	:
    else
	printf '%s/backups' $(tenant_dir)
    fi
}


tenant_dir()
{
    printf '%s' "${CID_TENANT_DIR}"
}

# End of file `tenant.sh'
