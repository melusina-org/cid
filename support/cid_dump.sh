#!/bin/sh

# cid_dump.sh — Operate on Cid repositories

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGE@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${subrdir:=@datadir@/subr}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${config_dir:=/opt/cid/var/config}
: ${backupdir:=${localstatedir}/backups}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"

. "${subrdir}/gitserver.sh"
. "${subrdir}/jenkins.sh"
. "${subrdir}/trac.sh"

#
# Ancillary functions
#

# dump_docker_exec SCRIPT
#  Exec SCRIPT in the trac docker container.
#
# The script runs as user ${dump_user}.

dump_docker_exec()
{
    docker exec -it --user "${dump_user}" "$(dump_delegate_subject)"\
           sh -c "$@"
}


# dump_docker_sh
#  Exec incoming SCRIPT in the trac docker container.
#
# The script runs as user ${dump_user}.

dump_docker_sh()
{
    docker exec -i --user "${dump_user}" "$(dump_delegate_subject)" sh
}



# dump_next_dumpname
#  The next available dumpname
#
# If the last dumpname is *.z the next avaialable dumpname is also
# *.z.  This allows for at most 26 backups a month.

dump_next_dumpname()
{
    local major minor script
    major=$(date +'%04Y-%02m-%02d')
    dump_next_dumpname__find "${dump_project}" "${major}"\
        | dump_next_dumpname__make "${dump_project}" "${major}"
}

dump_next_dumpname__find()
(
    cd "${backupdir}"
    find . -maxdepth 1 -name "$1.$2*" -type f
)


dump_next_dumpname__make()
{
    sed -e 's|^./||' \
        | sort -u\
        | awk -F '[.]' -v project="$1" -v major="$2" '
BEGIN {
 alphabet = "abcdefghijklmnopqrstuvwxyz"
 n = 0
}

{
  n = index(alphabet, $3)
}

END {
  if(n > 25) {
    n = 25
  }
  printf("%s.%s.%s\n", project, major, substr(alphabet, n+1, 1))
}
'
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
            *)	failwith 70 'cid_dump: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)
    config_setup
    next_dumpname="$(dump_next_dumpname)"
    wlog_prefix="dump: ${next_dumpname}"
    tmpdir_initializer
    exec 1> "${tmpdir}/cid_dump.log"
    wlog 'Info' 'Start the dump.'
    for service in ${config_service_list}; do
        when ${service}_is_enabled ${service}_dump "${next_dumpname}"
    done
    tar cJfC "${backupdir}/${next_dumpname}.txz" "${tmpdir}" .
    wlog 'Info' 'Dump complete.'
}

dump_main "$@"

# End of file `cid_dump.sh'
