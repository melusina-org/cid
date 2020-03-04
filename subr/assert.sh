### assert.sh -- A library to build test cases

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


assert_outcome='success'

assert_that()
{
    printf 'Assert that '
    printf "$@"
    printf ' … '
}

assert()
{
    if "$@"; then
	printf 'yes\n'
    else
	printf 'no\n'
	assert_outcome='failure'
    fi
}

assert_outcome()
{
    case "${assert_outcome}" in
	failure)
	    exit 1
	    ;;
	*)
	    exit 0
	    ;;
    esac
}

### End of file `assert.sh'
