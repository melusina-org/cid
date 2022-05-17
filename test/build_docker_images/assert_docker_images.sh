# assert_docker_images.sh — Assert that the docker images have been built

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


test_docker_image()
{
    docker image ls | grep -q -F "$1"
}

test_main()
{
    for service in 'linux' 'admin' 'gocd_server' 'gocd_agent'; do
	assert_that "the docker image 'cid/%s' has been built" "${service}"
	assert test_docker_image "cid/${service}"
    done
}

test_main

# End of file `assert_docker_images.sh'
