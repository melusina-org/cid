;;;; tenant.lisp — Tenant for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(defun populate-tenant-table ()
  "Populate the TENANT table with some test data."
  (let ((tenant-table-contents
	  '(("testsuite" "Test Suite")
	    ("acme" "Acme Inc."))))
    (loop for (pathname displayname) in tenant-table-contents
	  do (cid:make-tenant :pathname pathname :displayname displayname))))

(define-testcase testsuite-tenant ()
  (with-test-database
    (populate-tenant-table)
    (assert-string= "Test Suite"
		    (slot-value (cid:find-tenant "testsuite") 'cid::displayname))
    (assert-condition
	(cid:make-tenant :pathname "testsuite" :displayname "Test Suite")
	t)))

;;;; End of file `tenant.lisp'
