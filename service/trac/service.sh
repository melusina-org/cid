# service.sh — Service Definitions for trac

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

: ${tracdir:=/var/trac}

# trac_environment_location NAME
#  The web location for environment name

trac_environment_location()
{
    config "trac.environment.$1.location"\
        || printf '/trac/%s\n' "$1"
}


# trac_permission_db
#  The database of trac permssions
#
# This implements hardwired policy documents for the role subsystem.
#
# The permission database has the following columns:
#
#   ROLE | PERMISSION

trac_permission_db()
{
    cat <<EOF
admin|TRAC_ADMIN
developer|WIKI_ADMIN
developer|REPORT_ADMIN
developer|TICKET_MODIFY
EOF
}

trac_list_environments()
{
    if [ -d "${tracdir}/environment" ]; then
	find "${tracdir}/environment" -maxdepth 1 -mindepth 1\
	    | sed -e "s|${tracdir}/environment/||"
    fi
}


trac_admin()
{
    local project
    project="$1"
    shift
    {
	printf 'trac-admin %s/environment/%s' "${tracdir}" "${project}"
	printf ' %s' "$@"
    } | su -l www-data -s '/bin/sh'
}


# trac_create_environment NAME WWW-LOCATION
#  Prepare a trac environment
#
# This will skip the configuration if the environment has already been
# configured in a previous run.

trac_create_environment()
{
    local role permission environment location
    environment="$1"
    shift

    if [ $# -gt 0 ]; then
	location="$1"
	shift
    else
	location=$(trac_environment_location "${environment}")
    fi

    if [ -d "${tracdir}/environment/${environment}" ]; then
        wlog 'Info' 'trac: %s: Update trac environment.' "${environment}"
    else
        wlog 'Info' 'trac: %s: Create trac environment.' "${environment}"

        install -d -o www-data -g www-data -m 750\
                "${tracdir}/environment/${environment}"\
                "${tracdir}/sites/${environment}"\
                "${tracdir}/www/${environment}"

        install -d -o git -g git -m 750\
                "${tracdir}/git/${environment}"

	trac_admin "${environment}" initenv "${environment}" 'sqlite:db/trac.db'
    fi

    chown www-data:www-data "${tracdir}/www" .

    trac_admin "${environment}" deploy "${tracdir}/www/${environment}"

    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/${environment}.htpasswd"
    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/${environment}.conf"
    cat >> "${tracdir}/sites/${environment}.conf" <<SITE-CONF
Alias ${location}/chrome ${tracdir}/www/${environment}/htdocs

<Directory "${tracdir}/www/${environment}/htdocs">
  <IfModule mod_authz_core.c>
    Require all granted
  </IfModule>
</Directory>

<Location "${location}/login">
  AuthType Basic
  AuthName "Trac ${environment}"
  AuthUserFile ${tracdir}/sites/${environment}.htpasswd
  Require valid-user
</Location>

WSGIScriptAlias ${location} ${tracdir}/www/${environment}/cgi-bin/trac.wsgi
SITE-CONF

    trac_permission_db "${environment}" | while IFS='|' read role permission; do
	trac_admin "${environment}" permission add "${role}" "${permission}"
    done
}


# trac_delete_environment NAME
#  Delete a trac environment

trac_delete_environment()
{
    local environment
    environment="$1"
    shift

    if [ -d "${tracdir}/environment/${environment}" ]; then
        wlog 'Info' 'trac: %s: Delete trac environment.' "${environment}"
    else
        failwith 'trac: %s: Nothing is known about this environment.' "${environment}"
    fi

    rm -f\
       "${tracdir}/sites/${environment}.conf"\
       "${tracdir}/sites/${environment}.htpasswd"

    rm -Rf\
       "${tracdir}/environment/${environment}"\
       "${tracdir}/sites/${environment}"\
       "${tracdir}/www/${environment}"\
       "${tracdir}/git/${environment}"
}



#
# Behaviours
#

trac_wizard()
{
    wlog 'Info' 'trac wizard'
}


# trac_volume_database
#  List data volumes for trac environments

trac_volume_database()
{
    cat <<'EOF'
trac_data|/var/trac
EOF
}


# trac_configure
#  This creates trac environments specified by the main confguration file.

trac_configure()
{
    local environment location

    ln -s -f /service/trac/var/trac /var/trac

    wlog 'Info' "trac: Configure the persistent storage."
    chown www-data:www-data "${tracdir}"

    install -d -o root -g root -m 755\
            "${tracdir}"

    install -d -o www-data -g www-data -m 750\
            "${tracdir}/environment"\
            "${tracdir}/sites"\
            "${tracdir}/www"

    install -d -o git -g git -m 750\
            "${tracdir}/git"
}

# trac_list_users NAME
#  List in project NAME

trac_list_users()
{
    if [ -r "${tracdir}/sites/$1.htpasswd" ]; then
	awk -F':' '
{users[$1]}
END {
  for(user in users) {
    print(user)
  }
}' < "${tracdir}/sites/$1.htpasswd"
    fi
}


# trac_create_user NAME USER-NAME ROLE
#  Create user USER-NAME with the given ROLE in the project NAME

trac_create_user()
{
    local project username displayname role secret
    project="$1"
    username="$2"
    role="$3"

    printf 'trac: %s: Enter secret for user %s: ' "${project}" "${username}"
    stty -echo
    read secret
    stty echo
    printf '\n'
    wlog 'Info' 'trac: %s: Add user \047%s\047 to role \047%s\047.'\
         "${project}" "${username}" "${role}"
    trac_admin "${project}" permission add "${username}" "${role}"
    htpasswd -B -b "${tracdir}/sites/${project}.htpasswd" "${username}" "${secret}"
}


# trac_user_role NAME USER-NAME
#  The user role of USER-NAME in project NAME

trac_user_role()
{
    trac_admin "${project}" permission list\
	| awk -v username="${username}" '
$1 == username {
  print $2
}
/Available actions:/{
  exit(0)
}
'
}


# trac_delete_user NAME USER-NAME
#  Delete the user USER-NAME from project NAME

trac_delete_user()
{
    local project username displayname role secret
    project="$1"
    username="$2"

    htpasswd -D "${tracdir}/sites/${project}.htpasswd" "${username}"

    trac_admin "${project}" permission remove "${username}"\
	       $(trac_user_role "${project}" "${username}")
}


# trac_edit_environment NAME
#  Edit the configuration for project NAME

trac_edit_environment()
{
    if [ ! -f "${tracdir}/environment/$1/conf/trac.ini" ]; then
	failwith 'trac: %s: Cannot find configuration for this project.' "$1"
    fi
    nvi "${tracdir}/environment/$1/conf/trac.ini"
}

# trac_dump
#  Dump trac environments to dumpdir

trac_dump()
{
    local environment
    install -d -o www-data -g www-data "${CID_NEXT_DUMPDIR}/trac"

    wlog 'Info' 'trac: Copy trac sites.'
    (
	cd "${tracdir}" && find 'sites'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/trac"
    ) 2>&1

    wlog 'Info' 'trac: Copy trac webserver configuration.'
    (
	cd "${tracdir}" && find 'www'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/trac"
    ) 2>&1

    
    install -d -o www-data -g www-data "${CID_NEXT_DUMPDIR}/trac/environment"
    trac_list_environments | while read environment; do
        wlog 'Info' 'trac: %s: Copy trac environment.' "${environment}"
        trac-admin "${tracdir}/environment/${environment}"\
		   hotcopy "${CID_NEXT_DUMPDIR}/trac/environment/${environment}"
        chown -R www-data:www-data "${CID_NEXT_DUMPDIR}/trac"
    done
}

# trac_restore DUMPFILE
#  Restore trac environments

trac_restore()
{
    wlog 'Info' 'Restore trac sites and environments.'
    tar xJfC "$1" "${tracdir}" --strip-components 2 './trac/'
}

# End of file `service.sh'
