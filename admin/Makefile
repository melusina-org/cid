### Makefile -- El Cid tools

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


PROGRAM=		cid_configure.sh
PROGRAM+=		cid_console.sh
PROGRAM+=		cid_dump.sh
PROGRAM+=		cid_restore.sh

.for githook in ${PROGRAM:Mcid_githook_*}
BINDIR.${githook:R}=	${libexecdir}/cid
.endfor

.include "shell.prog.mk"

### End of file `Makefile'
