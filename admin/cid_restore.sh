#!/bin/sh

# cid_restore.sh — Operate on Cid repositories

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
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${subrdir:=@datadir@/subr}
: ${servicedir:=@datadir@/service}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${config_dir:=/opt/cid/var/config}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"
. "${subrdir}/tenant.sh"
. "${subrdir}/service.sh"

: ${CID_TENANT_DIR:=/opt/cid/var/config}

restore_run()
{
    service_do restore_run1
}

restore_run1()
{
    $1_restore "${dumpfile}"
}



# restore_main DUMP-NAME
#  Dump main

restore_main()
{
    local OPTIND OPTION OPTARG
    local dumpfile service

    OPTIND=1

    while getopts 'h' OPTION; do
        case ${OPTION} in
            h)	restore_usage; exit 0;;
            *)	failwith 70 'cid_restore: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    if [ $# -ne 1 ]; then
        failwith 64 "cid_restore: Can only restore exactly one dump file."
    fi
    dumpfile="$1"
    shift
    wlog_prefix="restore: ${dumpfile##*/}"

    service_load
    wlog 'Info' 'Start the restore operation.'
    restore_run
    wlog 'Info' 'Restore operation complete.'
}

restore_main "$@"

# End of file `cid_restore.sh'
