#!/bin/sh

# cid_configure.sh — Configure an El Cid Project

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2024 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${package:=@PACKAGE@}
: ${packagedir:=/@PACKAGE@}
: ${version:=@VERSION@}
: ${prefix:=@prefix@}
: ${subrdir:=@datadir@/subr}
: ${libexecdir:=@libexecdir@}
: ${localstatedir:=@localstatedir@}
: ${cachedir:=${localstatedir}/cache${packagedir}}

: ${config_dir:=/opt/cid/var/config}
: ${gnupgdir:=/home/cid/.gnupg}
: ${sshdir:=/home/cid/.ssh}
: ${wwwdir:=/var/www}
: ${ssldir:=/etc/ssl}

. "${subrdir}/stdlib.sh"
. "${subrdir}/config.sh"
. "${subrdir}/role.sh"

. "${subrdir}/gitserver.sh"
. "${subrdir}/jenkins.sh"
. "${subrdir}/trac.sh"

configure_config()
{
    git config --file "${config_dir}/cid.conf" "$@"
}

configure_assert()
{
    if ! [ -f "${config_dir}/cid.conf" ]; then
        failwith 70 '%s: File not found.' "${config_dir}/cid.conf"
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

# configure_ssl
#  Create a self-signed certificate.

configure_ssl()
{
  if [ -d "${configdir}/ssl" ]; then
      (
	  set -e
	  cd "${config_dir}/ssl"
	  find . | cpio -dump --owner 'cid:cid' "${ssldir}"
        )
  else
    generate_self_signed_certificate
  fi

  find "${ssldir}" -type f -exec chmod go= '{}' ';'
  find "${ssldir}" -type f -exec chown haproxy:haproxy '{}' ';'
}

generate_self_signed_certificate()
(
  local hostname
  hostname="$(configure_config project.hostname)"

  field()
  {
    printf '/%s=%s' "$@"
  }
  
  subject()
  {
    field 'C' 'FR'
    field 'ST' 'Isle-de-France'
    field 'L' 'Paris'
    field 'O' 'Melusina'
    field 'OU' 'Melusina Development'
    field 'CN' 'Melusina Development Platform'
  }
  
  openssl genrsa -out "${ssldir}/private/${hostname}.key" 2048
  openssl req\
        -new\
	-subj "$(subject)"\
	-addext "subjectAltName = DNS:${hostname}"\
	-key "${ssldir}/private/${hostname}.key"\
	-out "${ssldir}/private/${hostname}.csr"
  openssl x509\
        -req -days 365\
        -in "${ssldir}/private/${hostname}.csr"\
        -signkey "${ssldir}/private/${hostname}.key"\
        -out "${ssldir}/private/${hostname}.crt"
  cat\
      "${ssldir}/private/${hostname}.key"\
      "${ssldir}/private/${hostname}.crt"\
      > "${ssldir}/private/${hostname}.pem"
  cp\
        "${ssldir}/private/${hostname}.pem"\
	"${ssldir}/private/localhost.pem"
)

configure_batch()
{
    local service
    for service in ${config_service_list}; do
        when ${service}_is_enabled ${service}_configure
    done
}

#
# Main
#

config_setup
configure_assert
configure_ssl
configure_gpg
configure_ssh
configure_batch

# End of file `cid_configure.sh'
