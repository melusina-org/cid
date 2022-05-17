# lisp.sh — Interact with Lisp systems

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

lisp_safe_batch()
{
    sbcl --noinform\
         --no-userinit\
         --no-sysinit\
         --non-interactive\
         "$@"
}

lisp_unsafe_batch()
{
    sbcl --noinform\
         --non-interactive\
         "$@"
}

lisp_lifecycle_eval()
{
    local script
    script="$(cat)"
    lisp_batch\
        --load "${TOPLEVELDIR}/lisp/lifecycle/lifecycle.lisp"\
        --eval "(progn ${script})"
}

# End of file `lisp.sh'
