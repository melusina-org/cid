#!/bin/sh

# cid_dump.sh — Operate on Cid repositories

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
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${subrdir:=@datadir@/subr}
: ${servicedir:=@datadir@/service}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${config_dir:=/opt/cid/var/config}
: ${backupdir:=${localstatedir}/backups}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"
. "${subrdir}/tenant.sh"
. "${subrdir}/service.sh"

: ${CID_TENANT_DIR:=/opt/cid/var/config}
: ${CID_NEXT_DUMPNAME:=}
: ${CID_NEXT_DUMPDIR:=}

# dump_next_dumpname
#  The next available dumpname
#
# If the last dumpname is *.z the next avaialable dumpname is also
# *.z.  This allows for at most 26 backups a month.

dump_next_dumpname()
{
    local major minor script
    major=$(date +'%04Y-%02m-%02d')
    dump_next_dumpname__find "$(tenant_name)" "${major}"\
        | dump_next_dumpname__make "$(tenant_name)" "${major}"
}

dump_next_dumpname__find()
(
    cd "${backupdir}"
    find . -maxdepth 1 -name "$1.$2*" -type f
)


dump_next_dumpname__make()
{
    sort -u\
        | awk -F '[.]' -v tenant="$1" -v major="$2" '
BEGIN {
 alphabet = "abcdefghijklmnopqrstuvwxyz"
 n = 0
}

{
  sub("^[.]/" tenant "[.]" major "[.]", "")
  sub("[.].*$", "")
  n = index(alphabet, $0)
}

END {
  if(n > 25) {
    n = 25
  }
  printf("%s.%s.%s\n", tenant, major, substr(alphabet, n+1, 1))
}
'
}


# dump_next_dumpdir_initializer
#  Create a temporary directory
#
# The path to that directory is saved in dump directory. A hook is
# registered to remove that directory upon program termination.

dump_next_dumpdir_initializer()
{
    CID_NEXT_DUMPDIR=$(mktemp -d -t "${package}-XXXXXX")
    wlog 'Debug' 'dump_next_dumpdir_initializer: %s' "${tmpdir}"
    trap 'dump_next_dumpdir_reclaim' INT TERM EXIT
    export CID_NEXT_DUMPDIR
}

# dump_next_dumpdir_reclaim
#  Reclaim the created temporary directory

dump_next_dumpdir_reclaim()
{
    wlog 'Debug' 'dump_next_dumpdir_reclaim: %s' "${tmpdir}"
    rm -r -f "${CID_NEXT_DUMPDIR:?}"
}

dump_run()
{
    service_do dump_run1
}

dump_run1()
{
    $1_dump
}


# dump_main DUMP-NAME
#  Dump main

dump_main()
{
    local OPTIND OPTION OPTARG
    local dump_project next_dumpname service

    OPTIND=1
    dump_project='local'

    while getopts 'np:' OPTION; do
        case ${OPTION} in
            h)	dump_usage; exit 0;;
            p)	dump_project="${OPTARG}";;
            *)	failwith 64 'cid_dump: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    CID_NEXT_DUMPNAME="$(dump_next_dumpname)"
    wlog_prefix="dump: ${CID_NEXT_DUMPNAME}"
    dump_next_dumpdir_initializer
    service_load
    exec 1> "${CID_NEXT_DUMPDIR}/cid_dump.log"
    wlog 'Info' 'Start the dump operation.'
    dump_run
    tar cJfC "${backupdir}/${CID_NEXT_DUMPNAME}.txz" "${CID_NEXT_DUMPDIR}" .
    wlog 'Info' 'Dump operation complete.'
}

dump_main "$@"

# End of file `cid_dump.sh'
