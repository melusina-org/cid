### Makefile -- El Cid tools

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid
#
# Copyright © 2017 Michaël Le Barbier
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PROGRAM=		cid_repository.sh
PROGRAM+=		cid_githook_postreceive.sh

.for githook in ${PROGRAM:Mcid_githook_*}
BINDIR.${githook:R}=	${libexecdir}/cid
.endfor

.include "shell.prog.mk"

### End of file `Makefile'
