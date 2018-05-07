### users.sh -- Setup users

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

: ${users_package:=${PACKAGE}}
: ${users_configfile:=/root/setup/users.conf}

users_secure_variables='
 BLOCKSIZE
 CHARSET
 COLORFGBG
 COLORTERM
 DISPLAY XAUTHORIZATION XAUTHORITY
 EDITOR VISUAL
 HOME MAIL
 LANG
 LANGUAGE
 LC_ALL
 LC_COLLATE
 LC_CTYPE
 LC_MESSAGES
 LC_MONETARY
 LC_NUMERIC
 LC_TIME
 LINES COLUMNS
 LSCOLORS
 MAKEFLAGS
 MAKEOBJDIR
 MAKEOBJDIRPREFIX
 SSH_AUTH_SOCK
 TZ
'

is_true()
{
    case "$1" in
        [Yy][Ee][Ss]|[Tt][Rr][Uu][Ee])
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# users_secure_path
#  The secure path deduced from actual system configuration.
#
# This is designed to be used after setting up file hierarchies.

users_secure_path()
{
    if [ -f /etc/login.defs ]; then
    sed -n -e '
/^ENV_SUPATH/{
  s/.*PATH=//
  p
}
' /etc/login.defs
    else
        printf "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    fi
}


# users_edit_sudoers
#  Edit sudoers to add secure variables and secure path

users_edit_sudoers()
{
    sed -i -e "$(users_edit_sudoers__secure_variables)" /etc/sudoers
    sed -i -e "
s@secure_path=\".*@secure_path=\"$(users_secure_path)\"@
" /etc/sudoers
}

users_edit_sudoers__secure_variables()
{
    printf '/Defaults[[:space:]]*env_reset/a'
    printf '\134\nDefaults        env_keep += "%s"' ${users_secure_variables}
    printf '\n'
}

users_config()
{
    git config --file "${users_configfile}" "$@"
}

users_db()
{
    git config --file "${users_configfile}" --list\
        | awk -F '[.]' '
!($1 in s){
   s[$1]
   u[n++] = $1
}
END {
  for (i = 0; i < n; ++i) {
    print u[i]
  }
}
'
}

users_make()
{
    local username comment homedir createhome system shell
    local additionalusers additionalgroups
    local systemflag createhomeflag
    local additional
    local __varname__

    username="$1"
    for __varname__ in comment homedir createhome system shell additionalusers additionalgroups; do
        eval ${__varname__}=\$\(users_config "$1.${__varname__}"\)
    done

    if is_true "${createhome}"; then
        createhomeflag='--create-home'
    else
        createhomeflag=''
    fi

    if is_true "${system}"; then
        systemflag='--system'
    else
        systemflag=''
    fi


    useradd\
        --comment "${comment:-I am too lazy to document my users}"\
        --user-group\
        --home-dir "${homedir:-/home/$1}"\
        ${systemflag}\
        ${createhomeflag}\
        --shell "${shell:-/bin/bash}"\
        ${additionalgroups:+--groups }${additionalgroups}\
        "${username}"

    if [ -n "${additionalusers}" ]; then
        for additional in ${additionalusers}; do
            usermod -G "${username}" -a "${additional}"
        done
    fi
}

users_main()
{
    local username

    users_db | while read username; do
        users_make "${username}" || exit 1
    done || exit 1

    users_edit_sudoers
}


users_main "$@"

### End of file `users.sh'
