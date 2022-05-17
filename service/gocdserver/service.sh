# service.sh — Service Definitions for gocdserver

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

gocdserver_volume_database()
{
    cat <<'EOF'
gocdserver_home|/var/gocd_server/home/go
gocdserver_data|/var/gocd_server/godata
EOF
}

gocdserver_configure()
{
    chown -R go:go /var/gocd_server/*
}


gocdserver_dump()
{
    : "${CID_NEXT_DUMPDIR:?}"
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdserver"

    wlog 'Info' 'gocdserver: Dump home directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdserver/home/go"
    (
	cd "/var/gocd_server/home/go" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocdserver/home/go"
    ) 2>&1

    wlog 'Info' 'gocdserver: Dump data directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdserver/godata"
    (
	cd "/var/gocd_server/godata" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocdserver/godata"
    ) 2>&1
}

gocdserver_restore()
{
    wlog 'Info' 'gocdserver: Restore home directory.'
    tar xJfC "$1" '/var/gocd_server' --strip-components 2 './gocdserver/home/go'

    wlog 'Info' 'gocdserver: Restore data directory.'
    tar xJfC "$1" '/var/gocd_server' --strip-components 2 './gocdserver/godata'
}

gocdserver_wizard()
{
    local sshprivatekeydir sshprivatekeyfile tenant
    sshdir="${CID_TENANT_DIR}/ssh"
    sshprivatekeyfile="${sshdir}/id_rsa_gocd"
    tenant_name="$(config tenant.name)"

    if [ ! -d "${sshdir}" ]; then
	install -d "${sshdir}"
    fi

    if [ ! -f "${sshprivatekeyfile}" ]; then
	ssh-keygen -P '' -C "gocd@${tenant_name}" -f "${sshprivatekeyfile}"
    fi

    cat > "${sshdir}/config" <<EOF
Host github.com
  Hostname github.com
  User git
  StrictHostKeyChecking no
  IdentityFile ~/.ssh/id_rsa_gocd
  IdentitiesOnly yes
  UserKnownHostsFile=/dev/null

EOF

}

# End of file `service.sh'
