#!/bin/sh

# cid_jenkins_tool.sh — Support Jenkins Deployments

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

. "${subrdir}/jenkins.sh"


# tool_usage
#  Print usage information for the program

tool_usage()
{
    iconv -f utf-8 <<EOF
Usage: cid_jenkins_tool SUBCOMMAND
 Operate on a Jenkins installation
Subcommands:
 ls
 export
 import
Options:
 -h Display a help message.
EOF
}


# tool_main DUMP-NAME
#  Dump main

tool_main()
{
    local OPTIND OPTION OPTARG
    local tool_action
    OPTIND=1

    while getopts 'h:' OPTION; do
        case ${OPTION} in
            h)	tool_usage; exit 0;;
            *)	failwith 70 'cid_jenkins_tool: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)
    config_setup

    if ! jenkins_is_enabled; then
        wlog 'Error' 'cid_jenkins_tool: The Jenkins service is not enabled.\n'
        exit 1
    fi

    if [ $# = 0 ]; then
        wlog 'Error' 'cid_jenkins_tool: A subcommand is needed but none was provided.'
        tool_usage
        exit 64
    fi

    tool_action="$1"
    shift

    case "${tool_action}" in
        ls|export|import)
            "jenkins_job_${tool_action}" "$@"
            ;;
        *)
            tool_usage
            exit 64
            ;;
    esac
}

tool_main "$@"

# End of file `cid_jenkins_tool.sh'
