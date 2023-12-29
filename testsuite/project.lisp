;;;; project.lisp — Projects for El Cid

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

(defparameter *example-project-definitions*
  '((:pathname "testproject"
     :displayname "Test Project"
     :tenant "testsuite"))
  "Some project definitions that can be used in the testsuites.")

(defun example-project ()
  "Some project that can be used in the testsuite."
  (apply #'cid:make-project (first *example-project-definitions*)))

(defun populate-project-table ()
  "Populate the PROJECT table with some test data."
  (loop :for example :in *example-project-definitions*
	:do (apply #'cid:make-project example)))

(define-testcase testsuite-project ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (assert-string= "Test Project"
		    (cid:project-displayname (cid:find-project "testproject" "testsuite")))
    (assert-string= "Test Suite"
		    (cid:tenant-displayname (cid:project-tenant (cid:find-project "testproject" "testsuite"))))
    (assert-condition
	(cid:make-project :pathname "testproject" :displayname "Test Project 2" :tenant "testsuite")
	t)))

;;;; End of file `project.lisp'
