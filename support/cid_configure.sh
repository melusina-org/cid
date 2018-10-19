#!/bin/sh

### cid_configure -- Configure an El Cid Project

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

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGEDIR@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${subrdir:=@datadir@/subr}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}

: ${config_dir:=/opt/cid/var/config}
: ${gitdir:=/var/git}
: ${gnupgdir:=/home/cid/.gnupg}
: ${sshdir:=/home/cid/.ssh}
: ${wwwdir:=/var/www}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"
. "${subrdir}/trac.sh"

configure_config()
{
    git config --file "${config_dir}/cid.conf" "$@"
}

configure_assert()
{
    if ! [ -f "${config_dir}/cid.conf" ]; then
        failwith -x 70 '%s: File not found.' "${config_dir}/cid.conf"
    fi
}


# configure_git
#  Configure the git subsytem
#
# This makes sure the root of the filesystem holding the git
# repositories is owned by the git user.

configure_git()
{
    local keycomment

    chown 'git:git' "${gitdir}"

    install -d -o root -g root -m 755 "${gitdir}/.ssh"
    install -o root -g root -m 644 /dev/null "${gitdir}/.ssh/authorized_keys"
    if [ -d "${config_dir}/user" ]; then
       find "${config_dir}/user" -name 'authorized_keys' -exec awk '//' '{}' '+' \
            > "${gitdir}/.ssh/authorized_keys"
       awk '{print($3)}' "${gitdir}/.ssh/authorized_keys" | while read keycomment; do
           wlog 'Info' '%s: Authorized SSH-Key for git repositories.' "${keycomment}"
       done
    fi
}


# configure_gpg
#  Import GPG keys in in cid keyring
#
# This imports the GPG keys in "${config_dir}/gpg" in cid user keyring.
# If that directory does not exist, this phase is skipped.

configure_gpg()
{
    chown 'cid:cid' "${gnupgdir}"
    chmod '700' "${gnupgdir}"

    if [ -d "${config_dir}/gpg" ]; then
        su cid -c "
          find ${config_dir}/gpg -type f -exec gpg --no-use-agent --import '{}' ';'
        "
    fi
}


# configure_ssh
#  This imports a SSHconfiguration for the cid user.
#
# This imports the SSH configuration in "${config_dir}/ssh" in cid user
# account.  If that directory does not exist, this phase is skipped.

configure_ssh()
{
    chown 'cid:cid' "${sshdir}"
    chmod '700' "${sshdir}"

    if [ -d "${config_dir}/ssh" ]; then
        (
            set -e
            cd "${config_dir}/ssh"
            find . | cpio -dump --owner 'cid:cid' "${sshdir}"
        )

        find "${sshdir}" -type f -exec chmod go= '{}' ';'
    fi
}

configure_batch()
{
    configure_git
    when trac_is_enabled trac_configure
}

#
# Main
#

config_setup
configure_assert
configure_batch

### End of file `cid_configure.sh'
