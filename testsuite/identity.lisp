;;;; identity.lisp — Identity for El Cid

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

(defun populate-user-table ()
  "Populate the USER table with some test data."
  (let ((user-table-contents
	  '(("testuser" "Test User" "testsuite" :user)
	    ("testroot" "Test Root" "testsuite" :administrator))))
    (loop for (pathname displayname tenant role) in user-table-contents
	  do (cid:make-user :pathname pathname :displayname displayname :tenant tenant :role role))))

(define-testcase testsuite-user ()
  (with-test-database
    (populate-tenant-table)
    (populate-user-table)
    (assert-string= "Test User"
		    (cid:user-displayname (cid:find-user "testuser" "testsuite")))
    (assert-string= "Test Suite"
		    (cid:tenant-displayname (cid:user-tenant (cid:find-user "testuser" "testsuite"))))
    (assert-eq :user
	       (cid:user-role (cid:find-user "testuser" "testsuite")))
    (assert-string= "Test Root"
		    (cid:user-displayname (cid:find-user "testroot" "testsuite")))
    (assert-string= "Test Suite"
		    (cid:tenant-displayname (cid:user-tenant (cid:find-user "testroot" "testsuite"))))
    (assert-eq :administrator
	       (cid:user-role (cid:find-user "testroot" "testsuite")))
    (assert-condition
	(cid:make-user :pathname "testuser" :displayname "Test User" :tenant "testsuite")
	t)))

(define-testcase testsuite-identity ()
  (testsuite-user))

;;;; End of file `identity.lisp'
