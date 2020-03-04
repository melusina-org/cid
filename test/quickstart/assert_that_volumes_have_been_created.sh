### assert_that_the_volumes_have_been_created.sh.sh -- Assert that the docker volumes have been created

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

### End of file `assert_that_the_volumes_have_been_created.sh'
