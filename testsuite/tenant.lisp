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
	:do (apply #'cid:make-tenant example)))

(define-testcase tenant-unit-test ()
  (with-test-environment
    (populate-tenant-table)
    (flet ((ensure-that-find-tenant-returns-tenant-for-known-name ()
	     (assert-string=
	      "Test Tenant"
	      (cid:displayname (cid:find-tenant "testsuite"))))
	   (ensure-that-find-tenant-returns-nil-for-unknown-name ()
	     (assert-nil
	      (cid:find-tenant "This is not an actual tenant name")))
	   (ensure-that-make-tenant-with-taken-name-fails ()
	     (assert-condition
		 (cid:make-tenant :name "testsuite"
				  :displayname "Alternative Display Name")
		 error))
	   (ensure-that-describe-tenant-follows-the-expected-format ()
	     (assert-string-match
	      (with-output-to-string (*standard-output*)
		(describe (cid:find-tenant "testsuite")))
	      "*TENANT testsuite Test Tenant*")))
      (ensure-that-find-tenant-returns-tenant-for-known-name)
      (ensure-that-find-tenant-returns-nil-for-unknown-name)
      (ensure-that-make-tenant-with-taken-name-fails)
      (ensure-that-describe-tenant-follows-the-expected-format))))

;;;; End of file `tenant.lisp'
