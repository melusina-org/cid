# service.sh — Service Definitions for haproxy

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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
