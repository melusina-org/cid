### service.sh -- Service Definitions for gocdagent

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

gocdagent_volume_database()
{
    cat <<'EOF'
gocdagent_home|/var/gocd_agent/home/go
gocdagent_data|/var/gocd_agent/godata
EOF
}

gocdagent_configure()
{
    chown -R go:go /var/gocd_agent/*
}

gocdagent_dump()
{
    : "${CID_NEXT_DUMPDIR:?}"
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdagent"

    wlog 'Info' 'gocdagent: Dump home directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdagent/home/go"
    (
	cd "/var/gocd_agent/home/go" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocdagent/home/go"
    ) 2>&1

    wlog 'Info' 'gocdagent: Dump data directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocdagent/godata"
    (
	cd "/var/gocd_agent/godata" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocdagent/godata"
    ) 2>&1
}

gocdagent_restore()
{
    wlog 'Info' 'gocdagent: Restore home directory.'
    tar xJfC "$1" '/var/gocd_agent' --strip-components 2 './gocdagent/home/go'

    wlog 'Info' 'gocdagent: Restore data directory.'
    tar xJfC "$1" '/var/gocd_agent' --strip-components 2 './gocdagent/godata'
}


gocdagent_wizard()
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

### End of file `service.sh'