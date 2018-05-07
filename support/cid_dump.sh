#!/bin/sh

### cid_dump -- Operate on Cid repositories

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

# tmpdir_initializer
#  Create a temporary directory
#
# The path to that directory is saved in tmpdir. A hook is registered
# to remove that directory upon program termination.

tmpdir_initializer()
{
    tmpdir=$(mktemp -d -t "${package}-XXXXXX")
    wlog 'Debug' 'tmpdir_initializer: %s' "${tmpdir}"
    trap 'rm -r -f "${tmpdir:?}"' INT TERM EXIT
    export tmpdir
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

# dump_trac_ls
#  List trac environments

dump_trac_ls()
{
    find "${tracdir}" -maxdepth 1 -mindepth 1 -type d -not -name 'sites'\
        | sed -E -e "s|^${tracdir}/?||"
}

# dump_trac DUMP-NAME
#  Dump trac environments

dump_trac()
{
    local environment
    install -d -o www-data -g www-data "${tmpdir}/trac"

    wlog 'Info' '%s: Copy trac sites.' "$1"
    ( cd "${tracdir}" && find 'sites' | cpio -dump "${tmpdir}/trac" )\
        2>&1

    dump_trac_ls | while read environment; do
        wlog 'Info' '%s: %s: Copy trac environment.' "$1" "${environment}"
        trac-admin "${tracdir}/${environment}" hotcopy "${tmpdir}/trac/${environment}"
        chown -R www-data:www-data "${tmpdir}/trac"
    done
}


# dump_git_ls
#  List git repositories

dump_git_ls()
{
    find "${gitdir}" -name 'branches'\
        | sed -E -e "s|^${gitdir}/?||;s|/branches/?||"
}


# dump_git DUMP-NAME
#  Dump git repositories

dump_git()
{
    local repository
    install -d -o git -g git "${tmpdir}/git"
    dump_git_ls | while read repository; do
        wlog 'Info' '%s: %s: Clone repository.' "$1" "${repository}"
        git clone --quiet --bare "${gitdir}/${repository}" "${tmpdir}/git/${repository}"
        chown -R git:git "${tmpdir}/git"
    done
}

# dump_main DUMP-NAME
#  Dump main

dump_main()
{
    local OPTIND OPTION OPTARG
    local dump_project next_dumpname

    OPTIND=1
    dump_project='local'

    while getopts 'np:' OPTION; do
        case ${OPTION} in
            h)	dump_usage; exit 0;;
            p)	dump_project="${OPTARG}";;
            *)	failwith -x 70 'cid_dump: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)
    next_dumpname="$(dump_next_dumpname)"

    tmpdir_initializer
    exec 1> "${tmpdir}/cid_dump.log"
    wlog 'Info' '%s: Starting to dump.' "${next_dumpname}"
    dump_trac "${next_dumpname}"
    dump_git "${next_dumpname}"
    tar cJfC "${backupdir}/${next_dumpname}.txz" "${tmpdir}" .
    wlog 'Info' '%s: Dump complete.' "${next_dumpname}"
}

dump_main "$@"

### End of file `cid_dump.sh'
