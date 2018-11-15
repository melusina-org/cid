### @autoconf_input@

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

AC_INIT([cid_repository.sh])
AC_CONFIG_FILES([Makefile.config])

# AC_PATH_PROG_REQUIRE(VARIABLE-NAME, PROGRAM-NAME, HELP-MESSAGE)
# ---------------------------------------------------------------
# A variant of AC_PATH_PROG which fails if it cannot find its
# program.

AC_DEFUN([AC_PATH_PROG_REQUIRE],
[AC_PATH_PROG([$1], [$2], [no])dnl
 AS_IF([test "${$1}" = 'no'], [AC_MSG_ERROR([Program $2 not found. $3])], [])])

AC_PATH_PROG_REQUIRE([prog_cpio], [cpio])
AC_PATH_PROG_REQUIRE([prog_git], [git])
AC_PATH_PROG_REQUIRE([prog_docker], [docker])

AC_SUBST([runstatedir], [${prefix}/var/run])
AC_OUTPUT

### End of file `configure.ac'
