;;;; identity.lisp — Identity for El Cid

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

(defun populate-user-table ()
  "Populate the USER table with some test data."
  (let ((user-table-contents
	  '(("testuser" "Test User" "testsuite" :user)
	    ("testroot" "Test Root" "testsuite" :administrator))))
    (loop for (pathname displayname tenant role) in user-table-contents
	  do (cid:make-user :pathname pathname :displayname displayname :tenant tenant :role role))))

(define-testcase user-testsuite ()
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

(define-testcase identity-testsuite ()
  (user-testsuite))

;;;; End of file `identity.lisp'
