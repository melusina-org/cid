;;;; tenant.lisp — Tenant for El Cid

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

(defparameter *example-tenant-definitions*
  '((:name "testsuite"
     :displayname "Test Tenant")
    (:name "acme"
     :displayname "Acme Inc."))
  "Some tenant definitions that can be used in the testsuites.")

(defun example-tenant ()
  "Some tenant that can be used in the testsuite."
  (flet ((example-getf (indicator)
	   (getf (first *example-tenant-definitions*) indicator)))	   
    (cid:find-tenant (example-getf :name))))

(defun populate-tenant-table ()
  "Populate the TENANT table with some test data."
  (loop :for example :in *example-tenant-definitions*
	:for tenant = (apply #'cid:make-tenant example)
	:do (clsql:update-records-from-instance tenant)))

(define-testcase tenant-unit-test ()
  (with-test-database
    (populate-tenant-table)
    (assert-string=
     "Test Tenant"
     (cid:displayname (cid:find-tenant "testsuite")))
    (assert-condition
	(clsql:update-records-from-instance
	 (cid:make-tenant :name "testsuite"
			  :displayname "Alternative Display Name"))
	error)
    (assert-string-match
     (with-output-to-string (*standard-output*)
       (describe (cid:find-tenant "testsuite")))
     "*TENANT testsuite Test Tenant*")
    (assert-nil
     (cid:find-tenant "This is not an actual tenant name"))))

;;;; End of file `tenant.lisp'
