### jenkins.sh -- Jenkins interaction support

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

: ${jenkins_dir:=/var/lib/jenkins}


# jenkins_dump FILENAME
#  Dump Jenkins State and issue 

jenkins_dump()
{
    local environment
    install -d -o jenkins -g jenkins "${tmpdir}/jenkins"

    wlog 'Info' '%s: Copy Jenkins State.' "$1"
    ( cd "${jenkinsdir}" && find '.' | cpio -dump "${tmpdir}/jenkins" )\
        2>&1
}


### End of file `jenkins.sh'
