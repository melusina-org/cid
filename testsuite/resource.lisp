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

(define-testcase verify-steward-resource-relationships (resource)
  (flet ((verify-that-resource-scope-is-defined ()
	   (assert-t* (cid:tenant resource))
	   (assert-t* (cid:project resource))
	   (assert-t* (cid:steward resource)))
	 (verify-that-resource-and-steward-share-the-same-tenant (steward)
	   (assert-eq
	    (cid:tenant steward)
	    (cid:tenant resource)))
	 (verify-that-resource-and-steward-share-the-same-project (steward)
	   (assert-eq
	    (cid:project steward)
	    (cid:project resource))))
    (verify-that-resource-scope-is-defined)
    (with-slots ((steward cid:steward)) resource
      (assert-type steward 'cid:steward)
      (verify-that-resource-and-steward-share-the-same-tenant steward)
      (verify-that-resource-and-steward-share-the-same-project steward))))

(define-testcase verify-resource-lifecycle-invariants (resource &key slot-name new-slot-value)
  "Run a RESOURCE through its typical lifecycle.
The typical lifecycle of a resource is CREATE, EXAMINE, UPDATE, DELETE. In order
to update the resource we use SLOT-NAME and a NEW-SLOT-VALUE."
  (flet ((non-existent-resource-invariants (resource)
	   (assert-t* (cid:steward resource))
	   (assert-nil (cid:resource-exists-p resource))
	   (assert-nil (cid:resource-ready-p resource))
	   (assert-nil (cid:resource-identifier resource)))
	 (existent-resource-invariants (resource)
	   (assert-t (cid:resource-exists-p resource))
	   (assert-t (cid:resource-ready-p resource))
	   (assert-t* (cid:examine-resource resource))
	   (assert-t* (cid:resource-identifier resource))
	   (assert-type (cid:resource-identifier resource) 'string)
	   (assert-condition (cid:create-resource resource) cid:resource-already-exists)))
    (flet ((exercise-import-resource (resource)
	     (let ((imported-resource
		     (cid:import-resource (cid:steward resource) (type-of resource)
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
	   (exercise-modify-resource (resource)
	     (when slot-name
	       (setf (slot-value resource slot-name) new-slot-value)
	       (cid:update-resource-from-instance resource)))
	   (exercise-delete-resource (resource)
	     (cid:delete-resource resource)))
      (non-existent-resource-invariants resource)
      (cid:create-resource resource) 
      (existent-resource-invariants resource)
      (exercise-import-resource resource)
      (exercise-modify-resource resource)
      (existent-resource-invariants resource)
      (exercise-delete-resource resource)
      (non-existent-resource-invariants resource))))

(define-testcase ensure-that-not-created-resources-cannot-be-deleted (resource)
  (assert-t* (cid:steward resource))
  (assert-nil (cid:resource-exists-p resource))
  (assert-nil (cid:resource-ready-p resource))
  (assert-nil (cid:resource-identifier resource))
  (assert-condition (cid:delete-resource resource)
      simple-warning))

(define-testcase ensure-that-resources-are-created-only-once (resource)
  "Ensure that resources are created only once.
When CREATE-RESOURCE is called on an already created resource,
it is expected that an ERROR is signalled. This is because
we cannot guarantee that the existing resource is configured
according to the instance slots."
  (unwind-protect
       (progn (assert-t* (cid:steward resource))
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
		  cid:resource-already-exists))
    (cid:delete-resource resource)))

(define-testcase ensure-that-delete-signals-resource-no-longer-exists (resource)
  (cid:create-resource resource)
  (let ((imported-resource
	  (cid:import-resource (cid:steward resource) (type-of resource)
			       :displayname (cid:displayname resource)
			       :description (cid:description resource)
			       :identifier (cid:resource-identifier resource))))
    (cid:delete-resource imported-resource)
    (assert-condition (cid:delete-resource resource) cid:resource-no-longer-exists)))

(define-testcase resource-unit-test (&key make-resource slot-name new-slot-value resource-type)
  (verify-persistence-idempotency (funcall make-resource))
  (verify-steward-resource-relationships (funcall make-resource))
  (verify-resource-lifecycle-invariants (funcall make-resource)
					:slot-name slot-name
					:new-slot-value new-slot-value)
  (ensure-that-not-created-resources-cannot-be-deleted (funcall make-resource))
  (ensure-that-resources-are-created-only-once (funcall make-resource))
  (ensure-that-delete-signals-resource-no-longer-exists (funcall make-resource))
  (assert-type (funcall make-resource) 'cid:resource)
  (when resource-type
    (assert-type (funcall make-resource) resource-type)))

;;;; End of file `resource.lisp'
