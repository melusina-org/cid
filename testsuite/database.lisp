;;;; database.lisp — Database Connection for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use,
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

(in-package #:org.melusina.cid/testsuite)

(define-testcase testsuite-database ()
  (with-test-database
    (assert-t* (probe-file (testsuite-database-name)))
    (assert-t (clsql:table-exists-p "tenant")))
  (assert-nil (probe-file (testsuite-database-name))))

;;;; End of file `database.lisp'
