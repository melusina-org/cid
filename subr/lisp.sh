### lisp.sh – Interact with Lisp systems

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

lisp_safe_batch()
{
    sbcl --noinform\
         --no-userinit\
         --no-sysinit\
         --non-interactive\
         --disable-debugger\
         "$@"
}

lisp_unsafe_batch()
{
    sbcl --noinform\
         --non-interactive\
         --disable-debugger\
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

### End of file `lisp.sh'
