;;;; project.lisp — Projects for El Cid

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

(defun populate-project-table ()
  "Populate the PROJECT table with some test data."
  (let ((project-table-contents
	  '(("testproject" "Test Project" "testsuite"))))
    (loop for (pathname displayname tenant) in project-table-contents
	  do (cid:make-project :pathname pathname :displayname displayname :tenant tenant))))

(define-testcase project-testsuite ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (assert-string= "Test Project"
		    (cid:project-displayname (cid:find-project "testproject" "testsuite")))
    (assert-string= "Test Suite"
		    (cid:tenant-displayname (cid:project-tenant (cid:find-project "testproject" "testsuite"))))
    (assert-condition
	(cid:make-project :pathname "testproject" :displayname "Test Project" :tenant "testsuite")
	t)))

;;;; End of file `project.lisp'
