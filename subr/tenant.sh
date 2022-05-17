# tenant.sh — Functions for tenants

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This software is governed by the CeCILL-B license under French law and
# abiding by the rules of distribution of free software.  You can  use,
# modify and/ or redistribute the software under the terms of the CeCILL-B
# license as circulated by CEA, CNRS and INRIA at the following URL
# "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

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
