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
  (flet ((example-getf (indicator)
	   (getf (first *example-project-definitions*) indicator)))	   
    (cid:find-project
     (example-getf :pathname) :tenant (example-getf :tenant))))

(defun populate-project-table ()
  "Populate the PROJECT table with some test data."
  (loop :for example :in *example-project-definitions*
	:for project = (apply #'cid:make-project example)
	:do (clsql:update-records-from-instance project)))

(define-testcase testsuite-project ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (assert-string= "Test Project"
		    (cid:project-displayname
		     (cid:find-project "testproject"
				       :tenant "testsuite")))
    (assert-string= "Test Tenant"
		    (cid:tenant-displayname
		     (cid:project-tenant
		      (cid:find-project "testproject"
					:tenant "testsuite"))))
    (assert-t*
     (with-output-to-string (*standard-output*)
       (describe
	(cid:find-project "testproject"
			  :tenant "testsuite"))))
    (assert-eq (example-project)
	       (cid:find-project (example-project)
				 :tenant nil))
    (assert-condition
	(clsql:update-records-from-instance
	 (cid:make-project
	  :pathname "testproject"
	  :displayname "Test Project 2"
	  :tenant "testsuite"))
	error)))

;;;; End of file `project.lisp'
