# assert_docker_images.sh — Assert that the docker images have been built

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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
