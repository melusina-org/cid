# config.sh — Read Application configuration

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${config_file:=/dev/null}

# config CONFIGURATION-KEY
# config CONFIGURATION-KEY CONFIGURATION-VALUE
#  Get and set configuration values
#
# The configuration values are read from the file `config_file`

config()
{
    git config -f "${config_file}" "$@"
}


# config_db
#  Print the list of all configuration mappings
#
# These configuration mappings are delimited using the `|` character.

config_db()
{
    git config -f "${config_file}" --list | sed -e 's/=/|/'
}

# objdir
#  Compute the desired OBJDIR

objdir()
{
    local objdir localpwd
    localpwd="$(pwd)"
    localpwd="${localpwd#${HOME}}"
    localpwd="${localpwd#/}"
    if [ -n "${OBJDIR}" ]; then
	objdir="${OBJDIR}"
    else
	objdir="$(pwd)/obj"
    fi
    install -d "${objdir}"
    printf '%s' "${objdir}"
}

: ${OBJDIR:=$(objdir)}

# End of file `config.sh'
