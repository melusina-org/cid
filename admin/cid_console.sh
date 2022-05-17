#!/bin/sh

# cid_console.sh — Service Console

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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

# End of file `cid_console.sh'
