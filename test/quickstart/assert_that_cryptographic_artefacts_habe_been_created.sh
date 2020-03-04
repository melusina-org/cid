### assert_that_cryptographic_artefacts_habe_been_created.sh -- Assert that the docker volumes have been created

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




test_gocd_ssh_key()
{
    test -f "${CID_TENANT_DIR}/ssh/id_rsa_gocd"
}

test_ssl_certificate()
{
    test -f "${CID_TENANT_DIR}/ssl/${tenant}.bundle.pem"
}

test_main()
{
    assert_that 'gocd SSH key has been created'
    assert test_gocd_ssh_key

    assert_that 'SSL certifcate bundle has been created'
    assert test_ssl_certificate
}

test_main

### End of file `assert_that_cryptographic_artefacts_habe_been_created.sh'
