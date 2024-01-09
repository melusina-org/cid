;;;; property-list.lisp — Property List Stewards for El Cid

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

(define-testcase validate-steward-resource-relationships (resource)
  (assert-t* (cid:resource-steward resource))
  (with-slots ((steward cid:steward)) resource
    (assert-type steward 'cid:steward)
    (assert-string=
     (cid:tenant-pathname (cid:steward-tenant steward))
     (cid:tenant-pathname (cid:resource-tenant resource)))
    (assert-string=
     (cid:tenant-pathname (cid:steward-project steward))
     (cid:tenant-pathname (cid:resource-tenant resource)))
    (assert-string=
     (cid:tenant-pathname (cid:steward-project steward))
     (cid:tenant-pathname (cid:resource-project resource)))
    (assert-string=
     (cid:project-pathname (cid:steward-project steward))
     (cid:project-pathname (cid:resource-project resource)))
    (assert-string=
     (cid:steward-pathname (cid:resource-steward resource))
     (cid:steward-pathname resource))
    (assert-type steward (slot-value resource 'cid::steward-class))))

(define-testcase validate-property-list-property-lifecycle ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (let* ((testplist
	     (cid:make-property-list :tenant "testsuite"
				     :project "testproject"
				     :pathname "testplist"))
	   (property-1
	     (cid:make-property :property-list testplist
				:pathname "property-1"
				:value "Some value for property 1")))
      (validate-steward-resource-relationships property-1)
      (assert-type property-1 'cid:property))))

(define-testcase property-list-unit-test ()
  (validate-property-list-property-lifecycle)) 

;;;; End of file `property-list.lisp'
