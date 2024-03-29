;;;; phony.lisp — Property List Stewards for El Cid

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

(define-testcase phony-unit-test ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (let ((phony-steward
	    (cid:find-steward "phony"
			      :tenant "testsuite"
			      :project "testproject"
			      :steward-class 'cid:phony-steward)))
      (flet ((make-phony-resource ()
	       (cid:make-phony-resource :phony-steward phony-steward
					:name "phony-1"
					:displayname "Phony Resource #1"
					:description "A phony resource used in the testsuite.")))
	(resource-unit-test
	 :resource-type 'cid:phony-resource
	 :make-resource #'make-phony-resource)))))

;;;; End of file `phony.lisp'
