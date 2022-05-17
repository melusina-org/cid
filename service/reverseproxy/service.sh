# service.sh — Service Definitions for haproxy

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

reverseproxy_wizard()
{
    local tenant ssldir certfile keyfile

    tenant="$(config tenant.name)"
    ssldir="${CID_TENANT_DIR}/ssl"
    certfile="${ssldir}/${tenant}.cert.pem"
    keyfile="${ssldir}/${tenant}.clearkey.pem"
    bundlefile="${ssldir}/${tenant}.bundle.pem"

    if [ ! -d "${ssldir}" ]; then
	install -d "${ssldir}"
    fi

    if [ ! -f "${bundlefile}" ]; then
	openssl req -x509\
		-newkey rsa:4096 -nodes -keyout "${keyfile}"\
		-out "${certfile}"\
		-subj "/CN=${tenant}"\
		-days 365
	cat "${certfile}" "${keyfile}" > "${bundlefile}"
    fi
}

# End of file `service.sh'
