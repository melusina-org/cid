# hier.sh — Setup directory hierarchy

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${hier_package:=${PACKAGE}}

hier_directories='
 bin
 etc
 lib
 libexec
 man
 share
 var
 var/db
 var/distfiles
 var/log
 var/run
 var/src
'

hier_prefixes()
{
    cat <<EOF
/opt/local
/opt/${hier_package}
EOF
}


# hier_prefix PREFIX
#  Prepare directory hierarchy for PREFIX
#
# This creates the required directories and also arrange so that the
# corresponding *${prefix}/bin* directory is listed in the PATH
# variable.

hier_prefix()
{
    local prefix directory
    prefix="$1"

    install -d -m 755 "${prefix}"

    for directory in ${hier_directories}; do
        install -d -m 755 "${prefix}/${directory}";
        install -d -m 755 "${prefix}/${directory}/${hier_package}";
    done

    if [ -f /etc/login.defs ]; then
        sed -i -e "
/ENV_SUPATH\|ENV_PATH/{
 s@PATH=@PATH=${prefix}/bin:@
}
" /etc/login.defs
    fi

    if [ -f /etc/profile ]; then
        sed -i -e "
/PATH=\"/{
 s@PATH=\"@PATH=\"${prefix}/bin:@
}
" /etc/profile
    fi
}

# hier_main [-p PACKAGE] HIER-1 HIER-2 …
#  Prepare all required hierearchies
#
# When no hierarchies are provided, the list provided by
# $(hier_prefixes) is used.
#
#
# Options:
# -p PACKAGE
#    Set the package name, which governs the default list of
#    hierarchies.

hier_main()
{
    local OPTIND OPTARG OPTION prefix
    OPTIND=1

    while getopts "p:" OPTION; do
        case "${OPTION}" in
            p)	hier_package="${OPTARG}";;
            *)	exit 64;;
        esac
    done

    shift $((OPTIND - 1))

    if [ $# = 0 ]; then
        set -- $(hier_prefixes)
    fi
    for prefix in "$@"; do
        hier_prefix "${prefix}"
    done
}

hier_main "$@"

# End of file `hier.sh'
