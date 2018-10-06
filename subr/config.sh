### config.sh -- Read Application configuration

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

### End of file `config.sh'
