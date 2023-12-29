#!/bin/sh

# cid_admin.sh — Operate on Cid repositories

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGE@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${subrdir:=@datadir@/subr}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}

: ${tracdir:=/var/trac}
: ${gitdir:=/var/git}
: ${backupdir:=/var/backups}
: ${dockerimage:=cid/trac}

. "${subrdir}/stdlib.sh"

#
# Main
#

admin_user='root'
admin_project='local'
admin_backupdir='/var/backups'

# admin_usage
#  Print usage information for the program

admin_usage()
{
    iconv -f utf-8 <<EOF
Usage: cid_admin [-p PROJECT] SUBCOMMAND [ADMIN]
 Operate on cid repositories
Subcommands:
 ls
 config
 create
 delete
Options:
 -p PROJECT
 -h Display a help message.
EOF
}


admin_main()
{
    local OPTIND OPTION OPTARG subcommand mode project script

    subcommand='usage'
    mode='master'
    status=1
    OPTIND=1

    while getopts 'b:hp:t' OPTION; do
        case ${OPTION} in
            h)	admin_usage; exit 0;;
            b)	admin_backupdir="${OPTARG}";;
            p)	admin_project="${OPTARG}";;
            t)	admin_action='test';;
            *)	failwith 70 'cid_admin: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    admin_shell
}

admin_main "$@"

# End of file `cid_admin.sh'
