### quicklisp.sh -- Setup quicklisp

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This software is governed by the CeCILL-B license under French law and
# abiding by the rules of distribution of free software.  You can  use,
# modify and/ or redistribute the software under the terms of the CeCILL-B
# license as circulated by CEA, CNRS and INRIA at the following URL
# "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"


: ${PACKAGE:='cid'}
: ${quicklisp_package:=${PACKAGE}}
: ${quicklisp_quickstart:='/usr/share/cl-quicklisp/quicklisp.lisp'}
: ${quicklisp_home:="/opt/${quicklisp_package}/var/quicklisp"}
: ${quicklisp_local:="${quicklisp_home}/local-projects"}
: ${quicklisp_own:=root}
: ${quicklisp_group:=root}

#
# Install Quicklisp
#

quicklisp_install()
{
    local homedir
    eval homedir=~${quicklisp_own}

    trap '
     quicklisp_remove_install_script
    ' EXIT TERM KILL

    install -d -o "${quicklisp_own}" -g "${quicklisp_group}"\
            "${quicklisp_home}"

    quicklisp_prepare_install_script
    su - "${quicklisp_own}" -s /bin/sh -c 'sbcl --load /tmp/quicklisp-install.lisp'

    install -o "${quicklisp_own}" -g "${quicklisp_group}" /dev/null "${homedir}/.sbclrc"
    cat > "${homedir}/.sbclrc" <<EOF
#-:quicklisp
(let ((quicklisp-init #p"${quicklisp_home}/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
EOF
}

quicklisp_remove_install_script()
{
    rm -f /tmp/quicklisp-install.lisp
}

quicklisp_prepare_install_script()
{
    cat > /tmp/quicklisp-install.lisp <<EOF
(load #p"${quicklisp_quickstart}")
(quicklisp-quickstart:install :path "${quicklisp_home}/")
(quit)
EOF
}


#
# Load Packages
#

quicklisp_load()
{
    if [ $# -eq 0 ]; then
	return
    fi

    trap '
     : quicklisp_remove_load_script
    ' EXIT TERM KILL

    quicklisp_prepare_load_script "$@"
    su - "${quicklisp_own}" -s /bin/sh -c 'sbcl --load /tmp/quicklisp-load.lisp'
}

quicklisp_remove_load_script()
{
    rm -f /tmp/quicklisp-load.lisp
}

quicklisp_prepare_load_script()
{
    local packages

    packages=$(printf ' "%s"' "$@")
    packages="${packages# }"

    cat > /tmp/quicklisp-load.lisp <<EOF
#-:quicklisp
(let ((quicklisp-init #p"${quicklisp_home}/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload '(${packages}))
(quit)
EOF
}


#
# Register Local Projects
#

quicklisp_register_local_projects()
{
    trap '
     quicklisp_remove_register_script
    ' EXIT TERM KILL

    quicklisp_prepare_register_script

    install -d -o "${quicklisp_own}" -g "${quicklisp_group}"\
            "${quicklisp_local}"

    chown -R "${quicklisp_own}:${quicklisp_group}"\
          "${quicklisp_home}/local-projects"

    su - "${quicklisp_own}" -s /bin/sh -c 'sbcl --load /tmp/quicklisp-register.lisp'
}

quicklisp_remove_register_script()
{
    rm -f /tmp/quicklisp-register.lisp
}

quicklisp_prepare_register_script()
{
    cat > /tmp/quicklisp-register.lisp <<EOF
#-:quicklisp
(let ((quicklisp-init #p"${quicklisp_home}/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:register-local-projects)
(quit)
EOF
}

quicklisp_main()
{
    local OPTIND OPTARG OPTION action
    OPTIND=1

    action='all'

    while getopts "o:g:lri" OPTION; do
        case "${OPTION}" in
            o)	quicklisp_own="${OPTARG}";;
            g)	quicklisp_group="${OPTARG}";;
            l)	action='load';;
            r)	action='register';;
            i)	action='install';;
            *)	exit 64;;
        esac
    done

    shift $((OPTIND - 1))

    case "${action}" in
        all)
            quicklisp_install
            quicklisp_register_local_projects
            quicklisp_load "$@"
            ;;
        install)
            quicklisp_install
            ;;
        register)
            quicklisp_register_local_projects
            ;;
        load)
            quicklisp_load "$@"
            ;;
        *)
            exit 70
    esac
}

quicklisp_main "$@"
