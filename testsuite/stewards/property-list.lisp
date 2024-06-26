;;;; property-list.lisp — Property List Stewards for El Cid

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

(define-testcase property-list-unit-test ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (let ((testplist
	    (cid:make-property-list :tenant "testsuite"
				    :project "testproject"
				    :pathname "testplist")))
      (flet ((make-property ()
	       (let ((index (random 10000)))
		 (cid:make-property :property-list testplist
				    :pathname (format nil "property-~A" index)
				    :value (format nil "Some value for property ~A" index)))))
	(resource-unit-test
	 :resource-type 'cid:property
	 :make-resource #'make-property
	 :slot-name 'cid::value
	 :new-slot-value "Edited value for property")))))

;;;; End of file `property-list.lisp'
