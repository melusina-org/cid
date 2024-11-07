;;;; colima.lisp — Colima Steward for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase colima-unit-test ()
  ;(declare (test-size medium))
  (with-test-environment
    (populate-tenant-table)
    (populate-project-table)
    (let ((steward
	    (cid:make-colima-tool
	     :tenant "testsuite"
	     :project "testproject"
	     :name "local-colima-tool")))
      (flet ((make-colima-instance ()
	       (cid:make-colima-instance
		:colima-tool steward
		:name (string-downcase *testsuite-id*)
		:displayname "Testsuite Colima Instance"
		:description
		(format nil "A colima instance consumed by ~A." (string-downcase *testsuite-id*))
		:profile (string-downcase *testsuite-id*))))
	(resource-unit-test
	 :resource-type 'cid:colima-instance
	 :make-resource #'make-colima-instance)))))

;;;; End of file `colima.lisp'
