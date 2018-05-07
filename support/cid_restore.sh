#!/bin/sh

### cid_restore -- Operate on Cid repositories

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid
#
# Copyright © 2018 Michaël Le Barbier
#
# This file must be used under the terms of the MIT license.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at
# https://opensource.org/licenses/MIT

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGEDIR@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${tracdir:=/var/trac}
: ${gitdir:=/var/git}
: ${backupdir:=${localstatedir}/backups}


#
# Ancillary functions
#

# wlog PRINTF-LIKE-ARGV
#  Same as printf with output on STDERR
#
# A newline is automatically added to the output.

wlog_level='Info'

wlog__numeric_level()
{
    local level
    case "$1" in
        Emergency)      level=0;;
        Alert)          level=1;;
        Critical)       level=2;;
        Error)          level=3;;
        Warning)        level=4;;
        Notice)         level=5;;
        Info)           level=6;;
        Debug)          level=7;;
    esac
    printf '%s' "${level}"
}

wlog__is_interesting()
{
    [ $(wlog__numeric_level ${wlog_level}) -le $(wlog__numeric_level $1) ]
}

wlog()
{
    local level
    level="$1"
    shift

    if wlog__is_interesting "${level}"; then
        {
            printf '%s: ' "${level}"
            printf "$@"
            printf '\n'
        } 1>&2
    fi
}


# failwith [-x STATUS] PRINTF-LIKE-ARGV
#  Fail with the given diagnostic message
#
# The -x flag can be used to convey a custom exit status, instead of
# the value 1.  A newline is automatically added to the output.

failwith()
{
    local OPTIND OPTION OPTARG status

    status=1
    OPTIND=1

    while getopts 'x:' OPTION; do
        case ${OPTION} in
            x)	status="${OPTARG}";;
            *)	1>&2 printf 'failwith: %s: Unsupported option.\n' "${OPTION}";;
        esac
    done

    shift $(expr ${OPTIND} - 1)
    {
        printf 'Failure: '
        printf "$@"
        printf '\n'
    } 1>&2
    exit "${status}"
}


# restore_trac DUMPFILE
#  Dump trac environments

restore_trac()
{
    wlog 'Info' '%s: Restore trac sites and environments.' "$1"
    tar xJfC "$1" "${tracdir}" --strip-components 2 ./trac/
}


# restore_git DUMPFILE
#  Dump git repositories

restore_git()
{
    wlog 'Info' '%s: Restore git repositories.' "$1"
    tar xJfC "$1" "${gitdir}" --strip-components 2 ./git/
}

# restore_main DUMP-NAME
#  Dump main

restore_main()
{
    local OPTIND OPTION OPTARG
    local dumpfile

    OPTIND=1

    while getopts 'n' OPTION; do
        case ${OPTION} in
            h)	restore_usage; exit 0;;
            *)	failwith -x 70 'cid_restore: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    if [ $# -ne 1 ]; then
        failwith -x 64 "cid_restore: Can restore exactly one dump file."
    fi
    dumpfile="$1"
    shift

    wlog 'Info' '%s: Starting to restore.' "${dumpfile}"
    restore_trac "${dumpfile}"
    restore_git "${dumpfile}"
    wlog 'Info' '%s: Restore complete.' "${dumpfile}"
}

restore_main "$@"

### End of file `cid_restore.sh'
