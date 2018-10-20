### trac.sh -- Methods for the trac service

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

: ${tracdir:=/var/trac}


# trac_is_enabled
#  Predicate telling if the trac service is enabled

trac_is_enabled()
{
    config_service_is_enabled 'trac'
}

# trac_environment_db
#  The list of trac environments defined in the configuration file

trac_environment_db()
{
    config_db\
        | awk -F '|' '{print($1)}'\
        | awk -F '[.]' '$1 == "trac" && $2 == "environment" {s[$3]}END{for(t in s){print t}}'
}


# trac_environment_location NAME
#  The web location for environment name

trac_environment_location()
{
    config "trac.environment.$1.location"\
        || printf '/trac/%s\n' "$1"
}


# trac_configure
#  This creates trac environments specified by the main confguration file.

trac_configure()
{
    local environment trac_location

    wlog 'Info' "trac: Configure the persistent storage."
    chown www-data:www-data "${tracdir}"
    install -d -o www-data -g www-data -m 750\
            "${tracdir}/sites"\
            "${tracdir}/www"\
            "${tracdir}/git"

    trac_environment_db | while read environment; do
        wlog 'Info' "trac: %s: Configure the persistent storage for this environment."\
             "${environment}"
        trac_location=$(trac_environment_location "${environment}")
        trac_configure_environment "${environment}" "${trac_location}"
    done

}

# trac_configure_environment NAME WWW-LOCATION
#  Prepare a trac environment
#
# This will skip the configuration if the environment has already been
# configured in a previous run.

trac_configure_environment()
{
    if [ -d "${tracdir}/$1" ]; then
        wlog 'Info' 'trac: %s: Skip previously configured trac environment.' "$1"
        return 0
    else
        wlog 'Info' 'trac: %s: Configure trac environment.' "$1"
    fi

    install -d -o www-data -g www-data -m 750\
            "${tracdir}/$1"\
            "${wwwdir}/$1"

    install -d -o git -g git -m 750\
            "${gitdir}/$1"

    su -l www-data -s '/bin/sh' <<TRAC-ADMIN
trac-admin "${tracdir}/$1" initenv "$1" sqlite:db/trac.db
trac-admin "${tracdir}/$1" deploy "${wwwdir}/$1"
TRAC-ADMIN

    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/$1.htpasswd"
    install -o www-data -g www-data -m 640 /dev/null "${tracdir}/sites/$1.conf"
    cat >> "${tracdir}/sites/$1.conf" <<SITE-CONF
Alias $2/chrome ${wwwdir}/$1/htdocs

<Directory "${wwwdir}/$1/htdocs">
  <IfModule mod_authz_core.c>
    Require all granted
  </IfModule>
</Directory>

<Location "$2/login">
  AuthType Basic
  AuthName "Trac $1"
  AuthUserFile ${tracdir}/sites/$1.htpasswd
  Require valid-user
</Location>

WSGIScriptAlias $2 ${wwwdir}/$1/cgi-bin/trac.wsgi
SITE-CONF
}


# trac_dump
#  Dump trac environments to dumpdir

trac_dump()
{
    local environment
    install -d -o www-data -g www-data "${tmpdir}/trac"

    wlog 'Info' 'trac: Copy trac sites.'
    ( cd "${tracdir}" && find 'sites' | cpio -dump "${tmpdir}/trac" )\
        2>&1

    trac_environment_db | while read environment; do
        wlog 'Info' 'trac: %s: Copy trac environment.' "${environment}"
        trac-admin "${tracdir}/${environment}" hotcopy "${tmpdir}/trac/${environment}"
        chown -R www-data:www-data "${tmpdir}/trac"
    done
}

# trac_restore DUMPFILE
#  Restore trac environments

trac_restore()
{
    wlog 'Info' 'Restore trac sites and environments.'
    tar xJfC "$1" "${tracdir}" --strip-components 2 ./trac/
}

### End of file `trac.sh'
