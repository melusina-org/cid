# assert.sh — A library to build test cases

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

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

# End of file `assert.sh'
