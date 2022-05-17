#!/bin/sh

# cid_githook_postreceive.sh — Post receive repository hook

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


: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGEDIR@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}
: ${tracdir:=/var/trac}
: ${gitdir:=/var/git}

### LIBRARY

# tmpfile_initializer TMPFILE
#  Create a temporary file
#
# The path to that file is saved in TMPFILE. A hook is registered
# to remove that file upon program termination.

tmpfile_initializer()
{
    local _tmpfile _script
    _tmpfile=$(mktemp -t "${package}-XXXXXX")
    _script=$(printf 'rm -f "%s"' "${_tmpfile}")
    trap "${_script}" INT TERM EXIT
    eval $1="${_tmpfile}"
    export $1
}


# policy_trac_add_changeset
#
#   trac.addchangeset (false)
#   trac.environment (not-set)
#   trac.repositoryname (not-set)
#
# When trac.add_changeset is true, each received changeset is added to
# trac. This uses the corresponding command of trac-admin.

policy_trac_add_changeset()
(
    add_changeset=$(git config trac.addchangeset)
    environment=$(git config trac.environment)
    repository_name=$(git config trac.repositoryname)

    : ${add_changeset:=no}
    : ${environment:=${tracdir}/local}
    : ${repository_name:=default}

    if [ "${add_changeset}" = 'no' ]; then exit 0; fi

    while read oldrev newrev refname; do
        if [ "$oldrev" = 0000000000000000000000000000000000000000 ]; then
            git rev-list --reverse "${newrev}" --
        else
            git rev-list --reverse "${newrev}" "^${oldrev}" --
        fi | xargs sudo -u www-data trac-admin "${environment}" changeset added "${repository_name}"
    done
)

policy_trac_add_changeset


### IMPLEMENTATION

# Redirect output to stderr.
exec 2>>/var/log/cid_hithook_postreceive.log
exec 1>&2

set -x

# Save list of incoming commits in tmpfile
tmpfile_initializer postreceivedata
cat > "${postreceivedata}"

policy_trac_add_changeset < "${postreceivedata}"

# End of file `cid_githook_postreceive.sh'
