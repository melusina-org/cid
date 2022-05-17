# Makefile -- El Cid tools

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

PROGRAM=		cid_configure.sh
PROGRAM+=		cid_console.sh
PROGRAM+=		cid_dump.sh
PROGRAM+=		cid_restore.sh

PROGRAM+=		cid_githook_postreceive.sh

.for githook in ${PROGRAM:Mcid_githook_*}
BINDIR.${githook:R}=	${libexecdir}/cid
.endfor

.include "shell.prog.mk"

# End of file `Makefile'
