#!/bin/sh

### cid_repository -- Operate on Cid repositories

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid
#
# Copyright © 2017 Michaël Le Barbier
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGEDIR@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${tracdir:=/var/trac}
: ${gitdir:=/var/git}

# repository_make_compose_project_name PROJECT-NAME
#  Print the project name used by docker compose deduced from PROJECT-NAME

repository_make_compose_project_name()
{
    printf 'cid-%s' "$1" | sed -e 's/-//g'
}

repository_delegate_method()
{
    printf '%s' "${repository_delegate}"\
        | cut -d ':' -f 1
}

repository_delegate_subject()
{
    printf '%s' "${repository_delegate}"\
        | cut -d ':' -f 2
}

repository_docker_shell()
{
    cat <<EOF
/usr/local/bin/cid_repository${repository_project:+ -p }${repository_project} $@
EOF
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

repository_project='local'
repository_delegate='NATIVE:'

repository_main()
{
    local OPTIND OPTION OPTARG action mode project script

    action='help'
    mode='master'
    status=1
    OPTIND=1

    while getopts 'np:tS' OPTION; do
        case ${OPTION} in
            n)	repository_delegate="NATIVE:";;
            p)	repository_project="${OPTARG}"
                project=$(repository_make_compose_project_name "${OPTARG}")
                repository_delegate="DOCKER:${project}_trac_1";;
            t)	repository_delegate="TEST:";;
            S)	mode='slave';;
            *)	failwith -x 70 'cid_repository: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    if [ $# -eq 0 ]; then
        repository_usage -x 0
    fi

    action="$1"
    shift

    case "$(repository_delegate_method)" in
        NATIVE)
            repository_${action} "$@";;
        DOCKER)
            script=$(repository_docker_shell -n ${action} "$@")
            docker exec -it --user git "$(repository_delegate_subject)"\
                   sh -c "${script}"
            ;;
        TEST)
            repository_docker_shell ${action} "$@"
            ;;
        *)
            failwith -x 64 'cid_repository: %s: Delegation method unknown.' "$(repository_delegate_method)"
            ;;
    esac
}

repository_main "$@"

### End of file `cid_repository.sh'
