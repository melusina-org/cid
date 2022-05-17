# assert_that_volumes_have_been_created.sh — Assert that the docker volumes have been created

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

volume_list="
  ${tenant}.gocd_agent_data
  ${tenant}.gocd_agent_home
  ${tenant}.gocd_server_data
  ${tenant}.gocd_server_home
"

test_volume()
{
    docker volume ls | grep -q -F "$1"
}

test_main()
{
    for volume in ${volume_list}; do
	assert_that "volume '%s' has been created" "${volume}"
	assert test_volume "${volume}"
    done
}

test_main

# End of file `assert_that_volumes_have_been_created.sh'
