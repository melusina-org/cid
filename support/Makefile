### Makefile -- El Cid tools

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

PROGRAM=		cid_configure.sh
PROGRAM+=		cid_dump.sh
PROGRAM+=		cid_restore.sh
PROGRAM+=		cid_repository.sh
PROGRAM+=		cid_githook_postreceive.sh
PROGRAM+=		cid_jenkins_tool.sh

.for githook in ${PROGRAM:Mcid_githook_*}
BINDIR.${githook:R}=	${libexecdir}/cid
.endfor

.include "shell.prog.mk"

### End of file `Makefile'
