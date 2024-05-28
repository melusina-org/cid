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
    (let* ((delivery-stack
	     (poc:make-delivery-stack
	      :cloud-vendor (poc:make-cloud-vendor
			     :credential "ThisIsNotARealCredential")
	      :tag *testsuite-id*))
	   (delivery-resources
	     (slot-value delivery-stack 'poc::resources)))
      (loop :for resource :in delivery-resources
	      :do (verify-persistence-idempotency resource))
      (verify-persistence-idempotency delivery-stack))))

(define-testcase demonstrate-that-simple-resources-can-be-modified ()
  "Demonstrate that a simple resource can be modified.
A simple resource is a resource that has no prerequisites, while complex
resources with prerequisites can be seen as compounds."
  (with-test-environment
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
      (cid:create-resource resource)
      (let ((instructions
	      (cid:prepare-modification-instructions resource blueprint)))
	(assert-equal
	 instructions
	 (list
	  (list :update-instance resource
		'cid:name "vpc-1"
		'cid:displayname "First VPC")
	  (list :update-resource resource)))
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
	 (cid:resource-exists-p resource))))))

(define-testcase demonstrate-that-updating-an-immutable-field-recreates-the-resource ()
  (with-test-environment
    (let* ((certificate-authority
	     (poc:make-certificate-authority
	      :common-name "POC Root Certificate"
	      :public-key "ThisIsNotARealPublicKey"
	      :private-key "ThisIsNotARealPrivateKey"
	      :not-valid-before :today
	      :not-valid-after :today+1Y
	      :crl-distribution-points "URI:https://poc.local/crl"))
	   (resource
	     (poc:make-subject-certificate
	      :certificate-authority certificate-authority
	      :common-name "POC Subject Certificate"
	      :public-key "ThisIsNotARealPublicKey"
	      :not-valid-before :today
	      :not-valid-after :today+1W))
	   (blueprint
	     (poc:make-subject-certificate
	      :certificate-authority certificate-authority
	      :common-name "POC Subject Certificate"
	      :public-key "ThisIsNotARealPublicKey"
	      :not-valid-before :today
	      :not-valid-after :today+4W)))
      (cid:create-resource resource)
      (let ((instructions
	      (cid:prepare-modification-instructions resource blueprint)))
	(assert-equal
	 instructions
	 (list
	  (list :delete-resource resource)
	  (list :update-instance resource
		'poc:not-valid-after :today+4W)
	  (list :create-resource resource)))
	(cid:apply-modification-instructions instructions)
	(assert-eq
	 (slot-value resource 'poc:not-valid-after) :today+4W)
	(assert-t
	 (cid:resource-ready-p resource))
	(assert-t
	 (cid:resource-exists-p resource))))))

(define-testcase demonstrate-that-updating-a-prerequisite-does-not-touch-the-main-resource ()
  "Demonstrate that updating a prerequisite does not touch the main resource.
In this testcase we build a cluster with its private network prerequisite.
The NAME and DISPLAYNAME of the private network can be modified in-place,
which is what we do in the blueprint.

Under this scenario we expect only the network to be modified, even
if the modification request is on the cluster."
  (with-test-environment
    (let* ((cloud-vendor
	     (poc:make-cloud-vendor
	      :credential "ThisIsNotARealCredential"))
	   (network1
	     (poc:make-private-network
	      :cloud-vendor cloud-vendor
	      :name "vpc-1"
	      :displayname "First VPC"))
	   (network2
	     (poc:make-private-network
	      :cloud-vendor cloud-vendor
	      :name "vpc-2"
	      :displayname "Second VPC"))
	   (resource
	     (poc:make-container-cluster
	      :cloud-vendor cloud-vendor
	      :name "cluster"
	      :displayname "Cluster"
	      :private-network network1))
	   (blueprint
	     (poc:make-container-cluster
	      :cloud-vendor cloud-vendor
	      :name "cluster"
	      :displayname "Cluster"
	      :private-network network2)))
      (handler-bind ((cid:resource-prerequisite-is-missing
		       #'cid:create-missing-prerequisite))
	(cid:create-resource resource))
      (let ((instructions
	      (cid:prepare-modification-instructions resource blueprint)))
	(assert-equal
	 instructions
	 (list
	  (list :update-instance network1
		'cid:name "vpc-2"
		'cid:displayname "Second VPC")
	  (list :update-resource network1)))
	(handler-bind ((cid:resource-prerequisite-is-missing
			 #'cid:create-missing-prerequisite))
	  (cid:apply-modification-instructions instructions))
	(assert-string=
	 (cid:name blueprint)
	 (cid:name resource))
	(assert-string=
	 (cid:displayname blueprint)
	 (cid:displayname resource))
	(assert-t
	 (cid:resource-ready-p resource))
	(assert-t
	 (cid:resource-exists-p resource))))))

(define-testcase demonstrate-that-prerequisites-are-recreated-at-most-once ()
  "Demonstrate that prerequisites are recreated at most once.
In this testcase we build an infrastructure stack for software delivery
and we perform a modification of the cluster that forces it to be recreated.

In this scenario, we see that the network and the cluster is recreated
only once even if the cluster is a prerequisite of every service in
the stack."
  (with-test-environment
    (flet ((stack-network (stack)
	     (find-if #'(lambda (resource)
			  (typep resource 'poc:private-network))
		      (slot-value stack 'poc:resources)))
	   (stack-cluster (stack)
	     (find-if #'(lambda (resource)
			  (typep resource 'poc:container-cluster))
		      (slot-value stack 'poc:resources)))
	   (stack-service (stack image-repository)
	     (find-if #'(lambda (resource)
			  (and
			   (typep resource 'poc:container-service)
			   (string= image-repository
				    (slot-value
				     (slot-value resource 'poc:image)
				     'poc:repository))))
		      (slot-value stack 'poc:resources)))
	   (stack-load-balancer (stack)
	     (find-if #'(lambda (resource)
			  (typep resource 'poc:public-load-balancer))
		      (slot-value stack 'poc:resources))))
      (let* ((cloud-vendor
	       (poc:make-cloud-vendor
		:credential "ThisIsNotARealCredential"))
	     (resource
	       (poc:make-delivery-stack
		:cloud-vendor cloud-vendor
		:tag *testsuite-id*))
	     (network
	       (stack-network resource))
	     (cluster
	       (stack-cluster resource))
	     (blueprint
	       (let* ((delivery-stack
			(poc:make-delivery-stack
			 :cloud-vendor cloud-vendor
			 :tag *testsuite-id*))
		      (network
			(stack-network delivery-stack)))
		 (setf (slot-value network 'poc:availability-zone)
		       :az-2)
		 (values delivery-stack))))
	(handler-bind ((cid:resource-prerequisite-is-missing
			 #'cid:create-missing-prerequisite))
	  (cid:create-resource resource))
	(let ((instructions
		(cid:prepare-modification-instructions
		 (stack-load-balancer resource)
		 (stack-load-balancer blueprint))))
	  (assert-equal
	   instructions
	   (list
	    (list :delete-resource (stack-load-balancer resource))
	    (list :delete-resource (stack-service resource "cid/trac"))
	    (list :delete-resource (stack-service resource "cid/jenkins"))
	    (list :delete-resource (stack-service resource "cid/gitserver"))
	    (list :delete-resource cluster)
	    (list :delete-resource network)
	    (list :update-instance network
		  'poc:availability-zone :az-2)
	    (list :create-resource network)
	    (list :create-resource cluster)
	    (list :create-resource (stack-service resource "cid/gitserver"))
	    (list :create-resource (stack-service resource "cid/jenkins"))
	    (list :create-resource (stack-service resource "cid/trac"))
	    (list :create-resource (stack-load-balancer resource))))
	  (handler-bind ((cid:resource-prerequisite-is-missing
			   #'cid:create-missing-prerequisite))
	    (cid:apply-modification-instructions instructions))
	  (assert-t
	   (cid:resource-ready-p (stack-load-balancer resource)))
	  (assert-t
	   (cid:resource-exists-p (stack-load-balancer resource)))
	  (assert-eq
	   :az-2
	   (slot-value (stack-network resource) 'poc:availability-zone)))))))

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
  (demonstrate-that-simple-resources-can-be-modified)
  (demonstrate-that-updating-an-immutable-field-recreates-the-resource)
  (demonstrate-that-updating-a-prerequisite-does-not-touch-the-main-resource)
  (demonstrate-that-prerequisites-are-recreated-at-most-once)
  (demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated)
  (demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed)
  (demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments))

;;;; End of file `poc.lisp'
