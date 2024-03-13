#!/bin/sh

# cid_repository.sh — Operate on Cid repositories

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

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"

. "${subrdir}/trac.sh"
. "${subrdir}/gitserver.sh"


# repository_ls
#  List repositories for the given project

repository_ls()
{
    if ! [ -d "${gitserverdir}/${repository_environment}" ]; then
        return 0
    fi
    find "${gitserverdir}/${repository_environment}" \
         -maxdepth 1\
         -name '.*' -prune\
         -o\
         -type d -name '*.git' -print\
        | sed -e '
s|[.]git$||
s|.*/||
'
}

# repository_install_hook [REPO-1 …]
#  Install hoos for git REPO-1 or all repos

repository_install_hook()
{
    local repo

    if [ $# -le 0 ]; then
        repository_ls
    else
        printf '%s\n' "$@"
    fi | while read repo; do
        (
            cd "${gitserverdir}/${repository_environment}/${repo}.git" || exit 1
            if trac_is_enabled; then
                git config trac.addchangeset yes
            else
                git config trac.addchangeset no
            fi
            git config trac.environment "${tracdir}/${repository_environment}"
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
    if [ ! -d "${gitserverdir}/${repository_environment}/$1.git" ]; then
        failwith 'repository_config: %s: Nothing is known about this repository'\
                 "${repository_environment}/$1"
    else
        cd "${gitserverdir}/${repository_environment}/$1.git"
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

    envdir="${tracdir}/${repository_environment}"
    for repo in "$@"; do
        repodir="${gitserverdir}/${repository_environment}/${repo}.git"
        if [ -d "${repodir}" ]; then
            wlog 'Warning' '%s: Repository already exists.' "${repo}"
        else
            install -d -o git -g git -m 750 "${repodir}"
            (cd "${repodir}" && sudo -u git git init --bare . )
            sudo -u www-data trac-admin "${envdir}"\
                 repository add "${repo}" "${repodir}" 'git'
        fi
    done
    repository_install_hook "$@"
}

# repository_rm REPO-1 [REPO-2 …]
#  Delete a repository

repository_rm()
{
    local repo repodir envdir

    envdir="${tracdir}/${repository_environment}"
    for repo in "$@"; do
        repodir="${gitserverdir}/${repository_environment}/${repo}.git"
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
Usage: cid_repository [-t TRAC-ENVIRONMENT] SUBCOMMAND [REPOSITORY]
 Operate on cid repositories
Subcommands:
 ls
 config
 create
 rm
Options:
 -t TRAC-ENVIRONMENT
 -h Display a help message.
Description:
 A repository belongs to at most one trac environment.
EOF
}


repository_main()
{
    local OPTIND OPTION OPTARG
    local subcommand repository_environment

    repository_environment='NOT-SET'
    subcommand='usage'
    OPTIND=1

    while getopts 'ht:' OPTION; do
        case ${OPTION} in
            h)	repository_usage; exit 0;;
            t)	repository_environment="${OPTARG}";;
            *)	failwith 70 'cid_repository: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)
    config_setup

    if [ "${repository_environment}" = 'NOT-SET' ]; then
        if trac_is_enabled; then
            repository_environment=$(trac_environment_db | head -n 1)
        fi
    fi

    if [ $# -eq 0 ]; then
        repository_usage
        exit 64
    fi

    subcommand="$1"
    shift

    case "${subcommand}" in
        ls|config|create|rm|hook)
            : 'NOP'
            ;;
        *)
            failwith 64 'cid_repository: %s: Unknown subcommand.' "${subcommand}"
            ;;
    esac

    repository_${subcommand} "$@"
}

repository_main "$@"

# End of file `cid_repository.sh'
