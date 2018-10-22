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

: ${config_service_list:=gitserver trac}
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


# config_project
#  Determine the project name
#
# The project name is determined:
# - either from the variable config_project,
# - or from the configuration value project.name,
# - or from the name of the directory holding the configuration file.

config_project()
{
    local text
    if [ -n "${config_project}" ]; then
        printf '%s' "${config_project}"
    elif config 'project.name'; then
        : NOP
    else
        text="${config_file}"
        text="${text%/*.conf}"
        text="${text##*/}"
        printf '%s' "${text}"
    fi
}

# config_expand
#  Expand the HOME variable in environemt

config_expand()
{
    local sedscript
    sedscript=$(printf 's|[$][{]HOME[}]|%s|g;s|~|%s|g' "${HOME}" "${HOME}")
    sed -e "${sedscript}"
}

# config_statedir
#  The configure statedir

config_statedir()
{
    ( config 'project.statedir' | config_expand )\
        || printf '%s\n' "${statedir:-/var/backups}"
}

# config_backupdir
#  The configure backupdir

config_backupdir()
{
    ( config 'project.backupdir' | config_expand )\
        || printf '%s\n' "${backupdir:-/var/backups}"
}


# config_setup
#  Basic setup of configuration, exit on failure

config_setup()
{
    if [ -z "${config_dir}" ]; then
        failwith -x 70 'config.sh: config_dir: This variable is not set.'
    fi

    if ! [ -d "${config_dir}" ]; then
        failwith -x 64 '%s: Cannot read configuration directory.' "${config_dir}"
    fi

    config_file="${config_dir}/cid.conf"

    if ! [ -f "${config_file}" ]; then
        failwith -x 64 '%s: Cannot read configuration file.' "${config_file}"
    fi

    config_project="$(config_project)"
    config_backupdir="$(config_backupdir)"
    config_statedir="$(config_statedir)"
}


# config_service_is_enabled SERVICE
#  Predicate telling if the SERVICE is enabled

config_service_is_enabled()
{
    local enable
    enable=$(config "$1.service.enable")
    if [ "${enable}" = 'yes' ]; then
        wlog 'Info' '%s: This service is enabled.' "$1"
        return 0
    else
        wlog 'Info' '%s: This service is disabled.' "$1"
        return 1
    fi
}

# config_volume_db
#  Print volume database
#
# It has the columns
#
#     SERVICE | VOLUME-NAME | PREFERRED-MOUNT-POINT

config_volume_db()
{
    local service
    for service in ${config_service_list}; do
        if ${service}_is_enabled; then
            ${service}_volume_db "${config_project}"
        fi
    done
}

# config_volume_list
#  Print volume list

config_volume_list()
{
    config_volume_db | awk -F '|' '{s[$2]} END { for(n in s){print(n)}}'
}

### End of file `config.sh'
