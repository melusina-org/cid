# role.sh — Support for roles, users, groups and policies

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# We start with a simple model, where:
#
# - There is a finite list of possible roles. (admin, developer, anonymous)
# - Each role is attached to a hardwired policy-document.


# role_user_db NAME
#  Print the user database for environment NAME
#
# The user database has the following columns:
#
#  USERNAME | DISPLAYNAME | ROLE

role_user_db()
{
    local userdir userconf username displayname role

    if ! [ $# -eq 1 ]; then
        failwith 70 'role_user_db: Invalid call.'
    fi

    {
        if [ -d "${config_dir}/user" ]; then
            find "${config_dir}/user" -type d -maxdepth 1 -mindepth 1
        fi
    } | {
        while read userdir; do
            username="${userdir##*/}"
            displayname=$(role_user_config "${username}" 'displayname')
            role=$(role_user_config "${username}" 'role')
            if ( role_user_config "${username}" 'environment' | grep -q "$1\|[*]" ); then
                printf '%s|%s|%s\n' "${username}" "${displayname}" "${role}"
            fi
        done
    }
}


# role_user_config USER CONFIGURATION-KEY
#  Get and set configuration values
#
# The configuration values are read from the corresponding user.conf
# file or determined from the hardwired strategy when not set.

role_user_config()
{
    local userconf value
    if [ -f "${config_dir}/user/$1/user.conf" ]; then
        userconf="${config_dir}/user/$1/user.conf"
    else
        userconf='/dev/null'
    fi
    value=$(git config -f "${userconf}" "user.$1.$2")
    if [ -z "${value}" ]; then
        case "$2" in
            displayname)
                value="$1"
                ;;
            role)
                value='developer'
                ;;
            environment)
                value='*'
                ;;
        esac
    fi
    printf '%s' "${value}"
}

# role_user_secret USERNAME
#  Print the secret associated to USERNAME

role_user_secret()
{
    if [ -f "${config_dir}/user/$1/secret" ]; then
        cat "${config_dir}/user/$1/secret"
    else
        failwith 'role_user_secret: %s: User has no secret.' "$1"
    fi
}


# role_user_authorized_keys USERNAME
#  Print the authorized keys associated to USERNAME

role_user_authorized_keys()
{
    if [ -f "${config_dir}/user/$1/authorized_keys" ]; then
        # Using sed ensures the file ends with a newline character.
        sed -e '' "${config_dir}/user/$1/authorized_keys"
    else
        failwith 'role_user_authorized_keys: %s: User has no authorized keys.' "$1"
    fi
}

# End of file `role.sh'
