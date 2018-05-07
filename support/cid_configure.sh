#!/bin/sh

### cid_configure -- Configure an El Cid Project

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

: ${configdir:=/opt/cid/var/config}
: ${gitdir:=/var/git}
: ${gnupgdir:=/home/cid/.gnupg}
: ${sshdir:=/home/cid/.ssh}
: ${tracdir:=/var/trac}
: ${wwwdir:=/var/www}


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

configure_config()
{
    git config --file "${configdir}/cid.conf" "$@"
}

configure_environment_db()
{
    git config --file "${configdir}/cid.conf" --list\
        | awk -F '[.]' '{s[$1]}END{for(t in s){print t}}'
}


configure_assert()
{
    if ! [ -f "${configdir}/cid.conf" ]; then
        failwith -x 70 '%s: File not found.' "${configdir}/cid.conf"
    fi
}

configure_git()
{
    chown 'git:git' "${gitdir}"
}

configure_gpg()
{
    chown 'cid:cid' "${gnupgdir}"
    chmod '700' "${gnupgdir}"

    if [ -d "${configdir}/gpg" ]; then
        su cid -c "
          find ${configdir}/gpg -type f -exec gpg --no-use-agent --import '{}' ';'
        "
    fi
}

configure_ssh()
{
    chown 'cid:cid' "${sshdir}"
    chmod '700' "${sshdir}"

    if [ -d "${configdir}/ssh" ]; then
        (
            set -e
            cd "${configdir}/ssh"
            find . | cpio -dump --owner 'cid:cid' "${sshdir}"
        )

        find "${sshdir}" -type f -exec chmod go= '{}' ';'
    fi
}

configure_trac()
{
    local environment location

    chown www-data:www-data "${tracdir}"
    install -d -o www-data -g www-data -m 750\
            "${tracdir}/sites"\
            "${tracdir}/www"\
            "${tracdir}/git"

    configure_environment_db | while read environment; do
        location=$(configure_config "${environment}.location")
        : ${location:=/trac/${environment}}
        configure_trac_environment "${environment}" "${location}"
    done

}


# configure_trac_environment NAME WWW-LOCATION
#  Prepare a trac environment

configure_trac_environment()
{
    install -d -o www-data -g www-data -m 750\
            "${tracdir}/$1"\
            "${wwwdir}/$1"

    install -d -o git -g git -m 750\
            "${gitdir}/$1"

    su -l www-data -s '/bin/sh' <<TRAC-ADMIN
trac-admin "${tracdir}/$1" initenv "$1" sqlite:db/trac.db
trac-admin "${tracdir}/$1" deploy "${wwwdir}/$1"
TRAC-ADMIN

    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/$1.htpasswd"
    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/$1.conf"
    cat >> "${tracdir}/sites/$1.conf" <<SITE-CONF
Alias $2/chrome ${wwwdir}/$1/htdocs

<Directory "${wwwdir}/$1/htdocs">
  <IfModule mod_authz_core.c>
    Require all granted
  </IfModule>
</Directory>

<Location "$2/login">
  AuthType Basic
  AuthName "Trac $1"
  AuthUserFile ${tracdir}/sites/$1.htpasswd
  Require valid-user
</Location>

WSGIScriptAlias $2 ${wwwdir}/$1/cgi-bin/trac.wsgi
SITE-CONF
}


configure_all()
{
    configure_git
    configure_gpg
    configure_ssh
    configure_trac
}

#
# Main
#

configure_user='root'
configure_project='local'
configure_delegate='NATIVE:'


# configure_usage
#  Print usage information for the program

configure_usage()
{
    iconv -f utf-8 <<EOF
Usage: cid_configure [-p PROJECT] SUBCOMMAND [CONFIGURE]
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


configure_main()
{
    local OPTIND OPTION OPTARG subcommand mode project script

    subcommand='usage'
    mode='master'
    status=1
    OPTIND=1

    while getopts 'np:tS' OPTION; do
        case ${OPTION} in
            h)	configure_usage; exit 0;;
            n)	configure_delegate="NATIVE:";;
            p)	configure_project="${OPTARG}"
                project=$(configure_make_compose_project_name "${OPTARG}")
                configure_delegate="DOCKER:${project}_trac_1";;
            t)	configure_delegate="TEST:";;
            S)	mode='slave';;
            *)	failwith -x 70 'cid_configure: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)

    if [ $# -eq 0 ]; then
        configure_usage
        exit 64
    fi

    subcommand="$1"
    shift

    case "$(configure_delegate_method)" in
        NATIVE)
            configure_${subcommand} "$@";;
        DOCKER)
            script=$(configure_docker_script -n "${subcommand}" "$@")
            configure_docker_exec "${script}"
            ;;
        TEST)
            # configure_docker_script ${subcommand} "$@"
            configure_delegate="DOCKER:${project}_trac_1"
            configure_next_configurename
            ;;
        *)
            failwith -x 64 'cid_configure: %s: Delegation method unknown.' "$(configure_delegate_method)"
            ;;
    esac
}

configure_assert
configure_all
#configure_main "$@"

### End of file `cid_configure.sh'
