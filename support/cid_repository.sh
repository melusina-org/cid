#!/bin/sh

### cid_repository -- Operate on Cid repositories

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

# repository_ls
#  List repositories for the given project

repository_ls()
{
    find "${gitdir}/${repository_project}" \
         -maxdepth 1\
         -name '.*' -prune\
         -o\
         -type d -name '*.git' -print\
        | sed -e '
s|[.]git$||
s|.*/||
'
}

# repository_hook [REPO-1 …]
#  Install hoos for git REPO-1 or all repos

repository_hook()
{
    local repo

    if [ $# -le 0 ]; then
        repository_ls
    else
        printf '%s\n' "$@"
    fi | while read repo; do
        (
            cd "${gitdir}/${repository_project}/${repo}.git" || exit 1
            git config trac.addchangeset yes
            git config trac.environment "${tracdir}/${repository_project}"
            git config trac.repositoryname "${repo}"
            rm -f 'hooks/post-receive'
            ln -s "${libexecdir}/cid/cid_githook_postreceive" 'hooks/post-receive'
        )
    done
}

# repository_config REPO
#  Print the configuration of the given repo

repository_config()
{
    if [ ! -d "${gitdir}/${repository_project}/$1.git" ]; then
        failwith 'repository_config: %s: Nothing is known about this repository'\
                 "${repository_project}/$1"
    else
        cd "${gitdir}/${repository_project}/$1.git"
        shift
        if [ $# -le 0 ]; then
            set -- --list
        fi
        git config "$@"
    fi
}

# repository_create REPO-1 [REPO-2 …]
#  Create a new repository

repository_create()
{
    local repo repodir envdir

    envdir="${tracdir}/${repository_project}"
    for repo in "$@"; do
        repodir="${gitdir}/${repository_project}/${repo}.git"
        if [ -d "${repodir}" ]; then
            wlog 'Warning' '%s: Repository already exists.' "${repo}"
        else
            install -d -o git -g git -m 750 "${repodir}"
            (cd "${repodir}" && git init --bare . )
            sudo -u www-data trac-admin "${envdir}"\
                 repository add "${repo}" "${repodir}" 'git'
        fi
    done
    repository_hook "$@"
}

# repository_delete REPO-1 [REPO-2 …]
#  Delete a repository

repository_delete()
{
    local repo repodir envdir

    envdir="${tracdir}/${repository_project}"
    for repo in "$@"; do
        repodir="${gitdir}/${repository_project}/${repo}.git"
        sudo -u www-data trac-admin "${envdir}"\
             repository remove "${repo}"
        rm -Rf "${repodir:?}"
    done
}

#
# Main
#

# repository_usage
#  Print usage information for the program

repository_usage()
{
    iconv -f utf-8 <<EOF
Usage: cid_repository [-p PROJECT] SUBCOMMAND [REPOSITORY]
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


repository_main()
{
    local OPTIND OPTION OPTARG
    local subcommand repository_project

    repository_project='local'
    subcommand='usage'
    OPTIND=1

    while getopts 'p:' OPTION; do
        case ${OPTION} in
            h)	repository_usage; exit 0;;
            p)	repository_project="${OPTARG}";;
            *)	failwith -x 70 'cid_repository: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    if [ $# -eq 0 ]; then
        repository_usage
        exit 64
    fi

    subcommand="$1"
    shift

    case "${subcommand}" in
        ls|config|create|delete|hook)
            : 'NOP'
            ;;
        *)
            failwith -x 64 'cid_repository: %s: Unknown subcommand.' "${subcommand}"
            ;;
    esac

    repository_${subcommand} "$@"
}

repository_main "$@"

### End of file `cid_repository.sh'
