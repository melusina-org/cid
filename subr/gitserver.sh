### gitserver.sh -- Methods for the gitserver service

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

: ${gitserverdir:=/var/git}

# gitserver_is_enabled
#  Predicate telling if the gitserver service is enabled

gitserver_is_enabled()
{
    config_service_is_enabled 'gitserver'
}

# gitserver_repository_ls
#  List git repositories

gitserver_repository_ls()
{
    find "${gitserverdir}" -name 'branches'\
        | sed -E -e "s|^${gitserverdir}/?||;s|/branches/?||"
}


# gitserver_configure
#  Configure the git subsytem
#
# This makes sure the root of the filesystem holding the git
# repositories is owned by the git user.

gitserver_configure()
{
    local keycomment

    chown 'git:git' "${gitserverdir}"

    install -d -o root -g root -m 755 "${gitserverdir}/.ssh"
    install -o root -g root -m 644 /dev/null "${gitserverdir}/.ssh/authorized_keys"
    if [ -d "${config_dir}/user" ]; then
       find "${config_dir}/user" -name 'authorized_keys' -exec awk '//' '{}' '+' \
            > "${gitserverdir}/.ssh/authorized_keys"
       awk '{print($3)}' "${gitserverdir}/.ssh/authorized_keys" | while read keycomment; do
           wlog 'Info' '%s: Authorized SSH-Key for git repositories.' "${keycomment}"
       done
    fi
}


# gitserver_dump DUMP-NAME
#  Dump git repositories

gitserver_dump()
{
    local repository
    install -d -o git -g git "${tmpdir}/git"
    gitserver_repository_ls | while read repository; do
        wlog 'Info' '%s: %s: Clone repository.' "$1" "${repository}"
        git clone --quiet --bare "${gitserverdir}/${repository}" "${tmpdir}/git/${repository}"
        chown -R git:git "${tmpdir}/git"
    done
}


# gitserver_restore DUMPFILE
#  Restore git repositories

gitserver_restore()
{
    wlog 'Info' 'Restore git repositories.'
    tar xJfC "$1" "${gitserverdir}" --strip-components 2 ./git/
}


# gitserver_volume_db CONFIG-PROJECT
#  List data volumes for git repositories

gitserver_volume_db()
{
    cat <<EOF
gitserver|cid-$1-gitserver|/var/git
EOF
}

### End of file `gitserver.sh'
