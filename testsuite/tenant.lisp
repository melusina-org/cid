;;;; tenant.lisp — Tenant for El Cid

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

(defun populate-tenant-table ()
  "Populate the TENANT table with some test data."
  (let ((tenant-table-contents
	  '(("testsuite" "Test Suite")
	    ("acme" "Acme Inc."))))
    (loop for (pathname displayname) in tenant-table-contents
	  do (cid:make-tenant :pathname pathname :displayname displayname))))

(define-testcase tenant-testsuite ()
  (with-test-database
    (populate-tenant-table)
    (assert-string= "Test Suite"
		    (slot-value (cid:find-tenant "testsuite") 'cid::displayname))
    (assert-condition
	(cid:make-tenant :pathname "testsuite" :displayname "Test Suite")
	t)))

;;;; End of file `tenant.lisp'
