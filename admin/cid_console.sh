#!/bin/sh

### cid_console -- Service Console

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


: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGEDIR@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${subrdir:=@datadir@/subr}
: ${servicedir:=@datadir@/service}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}

: ${CID_TENANT_DIR:=/opt/cid/var/config}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"
. "${subrdir}/service.sh"
. "${subrdir}/tenant.sh"

console_assert()
{
    if ! [ -f "${CID_TENANT_DIR}/cid.conf" ]; then
        failwith 70 '%s: File not found.' "${CID_TENANT_DIR}/cid.conf"
    fi

    config_file="${CID_TENANT_DIR}/cid.conf"
}

console_main()
{
    console_assert
    service_load
    "$@"
}

console_main "$@"

### End of file `cid_console.sh'
