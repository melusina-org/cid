# assert_that_cryptographic_artefacts_habe_been_created.sh — Assert that the docker volumes have been created

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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

# End of file `assert_that_cryptographic_artefacts_habe_been_created.sh'
