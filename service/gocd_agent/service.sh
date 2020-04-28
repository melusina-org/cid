### service.sh -- Service Definitions for gocd_agent

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


gocd_agent_volume_database()
{
    cat <<'EOF'
gocd_agent_home|/var/gocd_agent/home/go
gocd_agent_data|/var/gocd_agent/godata
EOF
}

gocd_agent_configure()
{
    chown -R go:go /var/gocd_agent/*
}

gocd_agent_dump()
{
    : "${CID_NEXT_DUMPDIR:?}"
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocd_agent"

    wlog 'Info' 'gocd_agent: Dump home directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocd_agent/home/go"
    (
	cd "/var/gocd_agent/home/go" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocd_agent/home/go"
    ) 2>&1

    wlog 'Info' 'gocd_agent: Dump data directory.'
    install -d -o go -g go "${CID_NEXT_DUMPDIR}/gocd_agent/godata"
    (
	cd "/var/gocd_agent/godata" && find '.'\
		| cpio -dump "${CID_NEXT_DUMPDIR}/gocd_agent/godata"
    ) 2>&1
}

gocd_agent_restore()
{
    wlog 'Info' 'gocd_agent: Restore home directory.'
    tar xJfC "$1" '/var/gocd_agent' --strip-components 2 './gocd_agent/home/go'

    wlog 'Info' 'gocd_agent: Restore data directory.'
    tar xJfC "$1" '/var/gocd_agent' --strip-components 2 './gocd_agent/godata'
}


gocd_agent_wizard()
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
