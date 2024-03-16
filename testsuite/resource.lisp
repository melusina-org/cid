;;;; resource.lisp — Testing Resource Implementation for El Cid

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
  (assert-t* (cid:tenant resource))
  (assert-t* (cid:project resource))
  (assert-t* (cid:steward resource))
  (with-slots ((steward cid:steward)) resource
    (assert-type steward 'cid:steward)
    (block resource-and-steward-share-the-same-tenant
      (assert-string=
       (cid:name (cid:tenant steward))
       (cid:name (cid:tenant resource)))
      (assert-string=
       (cid::tenant-name (cid:project steward))
       (cid:name (cid:tenant resource))))
    (block resource-and-steward-share-the-same-project
      (assert-string=
       (cid::tenant-name (cid:project steward))
       (cid::tenant-name (cid:project resource)))
      (assert-string=
       (cid:name (cid:project steward))
       (cid:name (cid:project resource))))
    (block steward-joined-slot-is-consistently-set
      (assert-string=
       (cid:name (cid:steward resource))
       (cid::steward-name resource))
      (assert-type steward (slot-value resource 'cid::steward-class)))))

(define-testcase non-existent-resource-invariants (resource)
  (assert-t* (cid:steward resource))
  (assert-nil (cid:resource-exists-p resource))
  (assert-nil (cid:resource-ready-p resource))
  (assert-nil (cid:resource-identifier resource)))

(define-testcase existent-resource-invariants (resource)
  (assert-t (cid:resource-exists-p resource))
  (assert-t (cid:resource-ready-p resource))
  (assert-t* (cid:examine-resource resource))
  (assert-t* (cid:resource-identifier resource))
  (assert-type (cid:resource-identifier resource) 'string)
  (assert-condition (cid:create-resource resource)
      simple-error))

(define-testcase validate-resource-lifecycle (resource &key slot-name new-slot-value)
  "Run a RESOURCE through its typical lifecycle.
The typical lifecycle of a resource is CREATE, EXAMINE, UPDATE, DELETE. In order
to update the resource we use SLOT-NAME and a NEW-SLOT-VALUE."
  (non-existent-resource-invariants resource)
  (cid:create-resource resource)
  (existent-resource-invariants resource)
  (block import-resource
    (let ((imported-resource
	    (cid:import-resource (cid:steward resource) (type-of resource)
				 :name (cid:name resource)
				 :displayname (cid:displayname resource)
				 :description (cid:description resource)
				 :identifier (cid:resource-identifier resource))))
      (assert-equalp
       (cid:examine-resource resource)
       (cid:examine-resource imported-resource))
      (existent-resource-invariants imported-resource)
      (cid:delete-resource imported-resource)
      (non-existent-resource-invariants imported-resource)
      (cid:update-instance-from-resource resource)
      (non-existent-resource-invariants resource)
      (cid:create-resource resource)
      (existent-resource-invariants resource)))
  (block modify-resource
    (when slot-name
      (setf (slot-value resource slot-name) new-slot-value)
      (cid:update-resource-from-instance resource)
      (existent-resource-invariants resource)))
  (block delete-resource
    (cid:delete-resource resource)
    (non-existent-resource-invariants resource)))

(define-testcase ensure-that-resources-are-created-only-once (resource)
  "Ensure that resources are created only once.
When CREATE-RESOURCE is called on an already created resource,
it is expected that an ERROR is signalled. This is because
we cannot guarantee that the existing resource is configured
according to the instance slots."
  (assert-t* (cid:steward resource))
  (assert-nil (cid:resource-exists-p resource))
  (assert-nil (cid:resource-ready-p resource))
  (assert-nil (cid:resource-identifier resource))
  (cid:create-resource resource)
  (assert-t (cid:resource-exists-p resource))
  (assert-t (cid:resource-ready-p resource))
  (assert-t* (cid:examine-resource resource))
  (assert-t* (cid:resource-identifier resource))
  (assert-type (cid:resource-identifier resource) 'string)
  (assert-condition (cid:create-resource resource)
      simple-error))

(define-testcase ensure-that-not-created-resources-cannot-be-deleted (resource)
  (assert-t* (cid:steward resource))
  (assert-nil (cid:resource-exists-p resource))
  (assert-nil (cid:resource-ready-p resource))
  (assert-nil (cid:resource-identifier resource))
  (assert-condition (cid:delete-resource resource)
      simple-warning))

(define-testcase resource-unit-test (&key make-resource slot-name new-slot-value resource-type)
  (validate-steward-resource-relationships (funcall make-resource))
  (validate-resource-lifecycle (funcall make-resource)
			       :slot-name slot-name
			       :new-slot-value new-slot-value)
  (ensure-that-not-created-resources-cannot-be-deleted (funcall make-resource))
  (ensure-that-resources-are-created-only-once (funcall make-resource))
  (assert-type (funcall make-resource) 'cid:resource)
  (when resource-type
    (assert-type (funcall make-resource) resource-type)))

;;;; End of file `resource.lisp'
