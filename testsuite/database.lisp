;;;; database.lisp — Database Connection for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase database-unit-test ()
  (with-test-database
    (assert-t* (probe-file (testsuite-database-name)))
    (assert-t (clsql:table-exists-p "tenant")))
  (assert-nil (probe-file (testsuite-database-name))))

;;;; End of file `database.lisp'
