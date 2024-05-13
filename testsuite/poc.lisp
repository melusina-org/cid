;;;; poc.lisp — Validation for the proof of Concept

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

(define-testcase demonstrate-that-infrastructure-stack-can-be-created-and-destroyed ()
  "Demonstrate that an infrastructure stack can be created and destroyed.
This testcase prepares an infrastructure stack value, then creates
the corresponding resources and deletes them.

This is the smallest possible testcase for an infrastructure stack."
  (with-test-environment
    (populate-tenant-table)
    (populate-project-table)
    (flet ((check-that-resources-exist (resources)
	     (loop :for resource :in resources
		   :do (assert-t* (cid:resource-exists-p resource))))
	   (check-that-resources-do-not-exist (resources)
	     (loop :for resource :in resources
		   :do (assert-nil (cid:resource-exists-p resource))))
	   (stack-resources (stack)
	     (loop :for resource :in (slot-value stack 'poc::resources)
		   :append (cid:resource-prerequisites resource))))
      (let* ((delivery-stack
	       (poc:make-delivery-stack
		:cloud-vendor (poc:make-cloud-vendor
			       :credential "ThisIsNotARealCredential")
		:tag *testsuite-id*))
	     (delivery-resources
	       (stack-resources delivery-stack)))
	(cid:create-resource delivery-stack)
	(check-that-resources-exist delivery-resources)
	(cid:delete-resource delivery-stack)
	(check-that-resources-do-not-exist delivery-resources)))))

(define-testcase demonstrate-that-infrastructure-stack-can-be-persisted ()
  "Demonstrate that an infrastructure stack can be persisted.
This testcase prepares an infrastructure stack value, then persist it
to a file and read it back. This ensures that the state of
an infrastrcuture stack can be persisted, the lifespan of infrastructure
resources is usually longer than those of Common Lisp sessions."
  (with-test-environment
    (populate-tenant-table)
    (populate-project-table)
    (labels ((check-structural-equality (object1 object2)
	       (assert-equal (type-of object1) (type-of object2))
	       (etypecase object1
		 (string
		  (assert-string= object1 object2))
		 (symbol
		  (assert-eq object1 object2))
		 (list
		  (assert-equal (length object1) (length object2))
		  (loop :for item1 :in object1
			:for item2 :in object2
			:do (check-structural-equality item1 item2)))
		 ((or cid:tenant cid:project)
		  ;; TENANT and PROJECT instances are listed in a directory
		  ;; and must be physically equal rather than structurally equal.
		  (assert-eq object1 object2))
		 ((or poc:infrastructure-stack cid:steward cid:resource)
		  (assert-eq (cid:persistent-constructor (type-of object1))
			     (cid:persistent-constructor (type-of object2)))
		  (loop :for slot-spec :in (cid:persistent-slots object1)
			:for slot-name = (getf slot-spec :slot-name)
			:do (check-structural-equality
			     (slot-value object1 slot-name)
			     (slot-value object2 slot-name))))))
	     (write-then-read (object)
	       (cid:read-persistent-object-from-string
		(cid:write-persistent-object-to-string object)))
	     (check-persistence-idempotency (object)
	       (check-structural-equality object (write-then-read object)))
	     (stack-resources (stack)
	       (loop :for resource :in (slot-value stack 'poc::resources)
		     :append (cid:resource-prerequisites resource))))
      (let* ((delivery-stack
	       (poc:make-delivery-stack
		:cloud-vendor (poc:make-cloud-vendor
			       :credential "ThisIsNotARealCredential")
		:tag *testsuite-id*))
	     (delivery-resources
	       (stack-resources delivery-stack)))
	(loop :for resource :in delivery-resources
	      :do (check-persistence-idempotency resource))
	(check-persistence-idempotency delivery-stack)
	(check-structural-equality
	 delivery-stack
	 (progn
	   (cid:save-persistent-object delivery-stack *testsuite-id*)
	   (cid:load-persistent-object delivery-stack *testsuite-id*)))))))

(define-testcase demonstrate-that-simple-resources-can-be-modified ()
  "Demonstrate that a simple resource can be modified.
A simple resource is a resource that has no prerequisites, while complex
resources with prerequisites can be seen as compounds.

This testcase handles several case: the regular case, the case where
the attribute to modify is immutable, the case where the attribute to
modify has a value of type resource."
  (with-test-environment
    (flet ((regular-case ()
	     (let* ((cloud-vendor
		      (poc:make-cloud-vendor
		       :credential "ThisIsNotARealCredential"))
		    (resource
		      (poc:make-private-network
		       :cloud-vendor cloud-vendor
		       :name "vpc"
		       :displayname "Unique VPC"))
		    (blueprint
		      (poc:make-private-network
		       :cloud-vendor cloud-vendor
		       :name "vpc-1"
		       :displayname "First VPC")))
	       (flet ((validate-update-instance (instructions)
			(let ((instruction
				(first instructions)))
			  (assert-eq
			   :update-instance
			   (first instruction))
			  (assert-eq
			   resource
			   (getf instruction :update-instance))
			  (assert-string=
			   "vpc-1"
			   (getf instruction 'cid:name))
			  (assert-string=
			   "First VPC"
			   (getf instruction 'cid:displayname))
			  (assert-nil
			   (member 'cid::state instruction))
			  (assert-nil
			   (member 'cid::identifier instruction))))
		      (validate-update-resource (instructions)
			(let ((instruction
				(second instructions)))
			  (assert-eq
			   :update-resource
			   (first instruction))
			  (assert-eq
			   resource
			   (getf instruction :update-resource))
			  (assert-eq
			   2
			   (length instruction)))))
		 (cid:create-resource resource)
		 (let ((instructions
			 (cid:prepare-modification-instructions resource blueprint)))
		   (assert-eq 2 (length instructions))
		   (validate-update-instance instructions)
		   (validate-update-resource instructions)
		   (cid:apply-modification-instructions instructions)
		   (assert-string=
		    (cid:name blueprint)
		    (cid:name resource))
		   (assert-string=
		    (cid:displayname blueprint)
		    (cid:displayname resource))
		   (assert-t
		    (cid:resource-ready-p resource))
		   (assert-t
		    (cid:resource-exists-p resource)))))))
      (regular-case))))

(define-testcase demonstrate-that-infrastructure-stack-can-be-modified ()
  "Demonstrate that an infrastructure stack can be modified.
This testcase prepares an infrastructure stack value, then creates
the corresponding resources, modify an aspect of the stack and requires
the update of the stack.")

(define-testcase demonstrate-that-infrastructure-errors-can-be-resumed ()
  "Demonstrate that an infrastructure errors can be resumed
This testcase prepares an infrastructure stack value, configures
the laboratory so that resource creation then creates
the corresponding resources, modify an aspect of the stack and requires
the update of the stack.")

(define-testcase demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated ()
  "When we are about to recreate a stack that has already been created,
this is detected early and the condition can be restarted in an appropriate way")

(define-testcase demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed ()
  "When we are about to update the underlying resources of an infrastructure
stack, the changes about to be made can be reviewed before being performed.
This gives the operator the chance to interrupt the process if unexpected results are to be processed.")

(define-testcase demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments ()
  "Infrastructure stacks are promoted through environments.
An infrastructure stack has several instances and versions,
a given version can be promoted through computational environments
such as develelopment, staging, production.")

(define-testcase validate-poc ()
  "Validate our proof of concept."
  (demonstrate-that-infrastructure-stack-can-be-created-and-destroyed)
  (demonstrate-that-infrastructure-stack-can-be-persisted)
  (demonstrate-that-infrastructure-stack-can-be-modified)
  (demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated)
  (demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed)
  (demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments))

;;;; End of file `poc.lisp'
