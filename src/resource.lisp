;;;; resource.lisp — Resources for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

;;;;
;;;; Resource Class
;;;;

(clsql:def-view-class resource nil
  ((tenant-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader tenant-pathname)
   (project-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader project-pathname)
   (steward-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader steward-pathname)
   (steward-class
    :type symbol
    :allocation :class)
   (pathname
    :type string
    :initarg :pathname
    :initform (error "A RESOURCE requires a PATHNAME.")
    :documentation "A name for the RESOURCE.
The fully qualified name of the RESOURCE, which should be a safe Unix path. This PATHNAME
uniquely identifies the RESOURCE within the deployment it belongs to.")
   (description
    :type string
    :initarg :description
    :initform nil
    :documentation "A short description of the RESOURCE.")
   (state
    :type symbol
    :initarg :state
    :initform nil
    :documentation "The state of the resource.")
   (tenant
    :initarg :tenant
    :reader resource-tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-pathname
	      :foreign-key pathname
	      :set nil))
   (project
    :initarg :project
    :reader resource-project
    :db-kind :join
    :db-info (:join-class project
	      :home-key (tenant-pathname project-pathname)
	      :foreign-key (tenant-pathname pathname)
	      :set nil))
   (steward
    :initarg :steward
    :reader resource-steward
    :db-kind :join
    :db-info (:join-class steward
	      :home-key (tenant-pathname project-pathname steward-pathname)
	      :foreign-key (tenant-pathname project-pathname pathname)
	      :set nil))
   (identifier
    :type string
    :initarg :identifier
    :reader resource-identifier
    :initform nil
    :documentation
    "A text uniquely identifying the RESOURCE in the context of its STEWARD.
Depending on the RESOURCE and the STEWARD, this text can sometimes
not be determined automatically and requires the RESOURCE to be READ."))
  (:documentation "The class represents the state of resources required to a component deployment.
These resources can be created, read, updated and deleted. Resources can depend on other
resources. For a given STEWARD, any resource is uniquely identified by its IDENTIFIER slot."))

(defmethod print-object ((instance resource) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'tenant-pathname)
	       (slot-boundp instance 'project-pathname)
	       (slot-boundp instance 'steward-pathname)
	       (slot-boundp instance 'pathname)
	       (slot-boundp instance 'identifier))
      (with-slots (tenant-pathname project-pathname steward-pathname
		   pathname identifier)
	  instance
	(format stream "~A ~A ~A ~A ~A"
		tenant-pathname project-pathname steward-pathname
		pathname (or identifier "N/A"))))))

(defmethod describe-object ((instance resource) stream)
  (with-slots (description steward) instance
    (format stream "~&~A is a resource of type ~A provided by a ~A."
	    instance (type-of instance) (type-of steward))
    (format stream "~&Steward: ~A" steward)
    (when description
      (format stream "~&Description: ~A" description)))
  (values))

(defmethod initialize-instance :after ((instance resource)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((initialize-project-slot-from-steward-slot ()
	   (when (and (not (slot-boundp instance 'project))
		      (slot-boundp instance 'steward)
		      (typep (slot-value instance 'steward) 'steward))
	     (with-slots (steward) instance
	       (setf (slot-value instance 'project)
		     (steward-project steward)))))
	 (initialize-tenant-slot-from-project-slot ()
	   (when (and (not (slot-boundp instance 'tenant))
		      (slot-boundp instance 'project)
		      (typep (slot-value instance 'project) 'project))
	     (with-slots (project) instance
	       (setf (slot-value instance 'tenant)
		     (project-tenant project)))))
	 (finalize-tenant-slot ()
	   (when (slot-boundp instance 'tenant)
	     (with-slots (tenant) instance
	       (unless (typep tenant 'tenant)
		 (setf tenant (find-tenant tenant))))))
	 (finalize-tenant-pathname-slot ()
	   (when (and (slot-boundp instance 'tenant)
		      (not (slot-boundp instance 'tenant-pathname)))
	     (with-slots (tenant tenant-pathname) instance
	       (setf (slot-value instance 'tenant-pathname)
		     (tenant-pathname tenant)))))
	 (finalize-project-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-boundp instance 'tenant))
	     (with-slots (project tenant) instance
	       (unless (typep project 'project)
		 (setf project (find-project project :tenant tenant))))))
	 (finalize-project-pathname-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-value instance 'project)
		      (not (slot-boundp instance 'project-pathname)))
	     (with-slots (project) instance
	       (setf (slot-value instance 'project-pathname)
		     (project-pathname project)))))
	 (finalize-steward-slot ()
	   (when (slot-boundp instance 'steward)
	     (with-slots (steward) instance
	       (unless (typep steward 'steward)
		 (setf steward (find-steward steward))))))
	 (finalize-steward-pathname-slot ()
	   (when (and (slot-boundp instance 'steward)
		      (slot-value instance 'steward)
		      (not (slot-boundp instance 'steward-pathname)))
	     (with-slots (steward) instance
	       (setf (slot-value instance 'steward-pathname)
		     (steward-pathname steward)))))
	 (check-that-steward-and-steward-class-match ()
	   (with-slots (steward steward-class) instance
	     (unless (typep steward steward-class)
	       (error "Steward mismatch.
A resource of type ~A requires a ~A steward. However, the steward used
to initialise this resource was a ~A."
		      (type-of instance) steward-class (type-of steward))))))
    (initialize-project-slot-from-steward-slot)
    (initialize-tenant-slot-from-project-slot)
    (finalize-tenant-slot)
    (finalize-tenant-pathname-slot)
    (finalize-project-slot)
    (finalize-project-pathname-slot)
    (finalize-steward-slot)
    (finalize-steward-pathname-slot)
    (check-that-steward-and-steward-class-match)
    (values)))


;;;;
;;;; Resource Conditions
;;;;

(define-condition resource-error (error)
  ((operation
    :type symbol
    :initarg :operation
    :reader resource-error-operation)
   (resource
    :type resource
    :initarg :resource
    :reader resource-error-resource)
   (description
    :initarg :description
    :initform (error "A RESOURCE-ERROR must have a DESCRIPTION slot.")
    :reader resource-error-description
    :documentation "A short DESCRIPTION of the RESOURCE-ERROR.
This DESCRIPTION must be a short and generic description of the error. It must not
feature any detail specific to the operation or the resource, so that it is usually
safe to publish this DESCRIPTION. The DESCRIPTION must be unique within all other
error DESCRIPTIONS used by the same steward.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer explanation describing the RESOURCE-ERRROR.
This longer EXPLANATION is a text that could be presented to the console
operator.  It is allowed to feature details specific to the operation or
the resource, so that it is usually unsafe to publish this EXPLANATION."))
  (:report
   (lambda (condition stream)
     (with-slots (pathname steward) (resource-error-resource condition)
       (let ((*print-circle* nil))
	 (format stream "~&Operation on resource ~A failed.

The steward ~A trying to ~A the resource ~A met an error condition.
~A" 
		 pathname
		 (slot-value (find-steward steward) 'pathname)
		 (resource-error-operation condition) pathname
		 (resource-error-description condition))
	 (with-slots (explanation) condition
	   (when explanation
	     (format stream "~&~A" explanation)))))))
  (:documentation
   "This condition is signaled when a steward operating a resource meets an error condition."))

(defun resource-error (operation resource description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-ERROR."
  (signal 'resource-error
	  :operation operation
	  :resource resource
	  :description description
	  :explanation (when control-string
			 (apply #'format nil control-string format-arguments))))

(define-condition resource-confirmation (serious-condition)
  ((operation
    :type symbol
    :initarg :operation
    :reader resource-confirmation-operation)
   (resource
    :type resource
    :initarg :resource
    :reader resource-confirmation-resource)
   (description
    :initarg :description
    :initform (error "A RESOURCE-CONFIRMATION must have a DESCRIPTION slot.")
    :reader resource-confirmation-description
    :documentation "A short DESCRIPTION of the RESOURCE-CONFIRMATION.
This DESCRIPTION must be a short and generic description of the confirmation. It must not
feature any detail specific to the operation or the resource, so that it is usually
safe to publish this DESCRIPTION. The DESCRIPTION must be unique within all other
confirmation DESCRIPTIONS used by the same steward.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer explanation describing the RESOURCE-ERRROR.
This longer EXPLANATION is a text that could be presented to the console
operator.  It is allowed to feature details specific to the operation or
the resource, so that it is usually unsafe to publish this EXPLANATION."))
  (:report
   (lambda (condition stream)
     (with-slots (pathname steward) (resource-confirmation-resource condition)
       (let ((*print-circle* nil))
	 (format stream "Operation on resource ~A requires confirmation.

The steward ~A is trying to ~A the resource ~A and requires confirmation to proceed.
~A" 
		 pathname
		 (slot-value (find-steward steward) 'pathname)
		 (resource-confirmation-operation condition) pathname
		 (resource-confirmation-description condition))
	 (with-slots (explanation) condition
	   (when explanation
	     (format stream "~&~A" explanation)))))))
  (:documentation
   "This condition is signaled when a steward operating a resource requires confirmation."))

(defun resource-confirmation (operation resource description
			      &optional control-string &rest format-arguments)
  "Signal a RESOURCE-CONFIRMATION with restarts.
When the operation is approved, this form returns NIL. When the operation is refused,
the RESOURCE-CONFIRMATION condition is converted into a RESOURCE-ERROR with the same
parameters."
  (restart-case
      (error 'resource-confirmation
	     :operation operation
	     :resource resource
	     :description description
	     :explanation (when control-string
			    (apply #'format nil control-string format-arguments)))
    (approve-operation ()
      :report "Approve operation on resource"
      nil)
    (refuse-operation ()
      :report "Refuse operation on resource"
      (error 'resource-error
	     :operation operation
	     :resource resource
	     :description description
	     :explanation (when control-string
			    (apply #'format nil control-string format-arguments))))))

(defmacro with-resource-confirmation (approve-p &body forms)
  "Run body FORMS in an environment with automatic resource confirmation.
When the value of the generalised boolean APPROVE-P is a truthy value, then
the APPROVE-OPERATION restart is selected. When this value is NIL, the REFUSE
operation is selected."
  `(handler-bind
       ((resource-confirmation
	  (lambda (c)
	    (declare (ignore c))
	    (if ,approve-p
		(invoke-restart 'approve-operation)
		(invoke-restart 'refuse-operation)))))
     ,@forms))


;;;;
;;;; Resource Lifecycle Methods
;;;;

(defgeneric resource-exists-p (resource)
  (:documentation "Predicate recognising RESOURCES that exists.
The resource might exist but not be in a ready state, see also
the RESOURCE-READY-P predicate.")
  (:method ((instance resource))
    (not (eq nil (slot-value instance 'state)))))

(defgeneric resource-ready-p (resource)
  (:documentation "Predicate recognising RESOURCES in a ready state.")
  (:method ((instance resource))
    (eq t (slot-value instance 'state))))

(defgeneric create-resource (resource)
  (:documentation "Create a RESOURCE using its steward.
This returns the RESOURCE instance."))

(defmethod create-resource :around ((instance resource))
  "Enforce calling convention and ensure that we do not recreate a resource that already exists."
  (with-slots (pathname identifier state) instance
    (when state
      (error "Cannot create the resource ~A (~A) as it already exists." pathname identifier)
      (return-from create-resource instance)))
  (call-next-method)
  (values instance))

(defgeneric delete-resource (resource)
  (:documentation "Delete a RESOURCE using its steward.
This returns the RESOURCE instance."))

(defmethod delete-resource :around ((instance resource))
  "Enforce calling convention and ensure that we do not delete a resource that does not exist."
  (with-slots (pathname identifier state) instance
    (unless state
      (warn "Cannot delete the resource ~A (~A) as it does not exist." pathname identifier)
      (return-from delete-resource instance))
    (call-next-method)
    (when (eq state nil)
      (setf identifier nil)))
  (values instance))

(defgeneric update-instance-from-resource (instance)
  (:documentation "Update INSTANCE slots with attributes from resource.
The underlying resource is examined using the steward of the instance
and the instance slots are updated to reflect the state of the resource.

If the resource can be found in the context of its STEWARD based
on its IDENTIFIER, then RESOURCE slots are updated and the RESOURCE
is returned.

If the resource cannot be found in the context of its STEWARD based
on its IDENTIFIER, then the IDENTIFIER is wiped and NIL is returned."))

(defmethod update-instance-from-resource :around ((instance resource))
  "Enforce calling convention."
  (call-next-method)
  (values instance))

(defgeneric update-resource-from-instance (instance)
  (:documentation "Update resource attributes to reflect INSTANCE.
The attributes of the underlying resource are examined and when differing
from the attributes of INSTANCE, the resource is modified to reflect
the state of instance. Modifying the underlying resource could imply
recreating it."))

(defmethod update-resource-from-instance :around ((instance resource))
  "Enforce calling convention."
  (call-next-method)
  (values instance))

(defgeneric examine-resource (resource)
  (:documentation "Dump the state of a RESOURCE.
The result is a property list, mapping keywords to atoms. These
keywords are sorted in ascending order.")
  (:method-combination append))

(defmethod examine-resource :around ((instance resource))
  "Ensure the results of examining a resource are sorted."
  (sort-plist (call-next-method)))

(defmethod examine-resource append ((instance resource))
  (with-slots (steward pathname identifier description state) instance
    (list
     :steward (steward-pathname steward)
     :pathname pathname
     :identifier identifier
     :description description
     :state state)))

(defgeneric import-resource (steward &key pathname description identifier)
  (:documentation
   "Import a RESOURCE based on its IDENTIFIER into its STEWARD.
This assumes a resource has been created in STEWARD by a third party and
imports it into the current system by creating a resource for it."))

(defmethod import-resource :around (steward &key pathname description identifier)
  (let ((instance
	  (call-next-method)))
    (macrolet ((setf-slot-value (slot-name)
		 `(setf (slot-value instance ',slot-name) ,slot-name)))
      (setf-slot-value pathname)
      (setf-slot-value description)
      (setf-slot-value identifier)
      (setf-slot-value steward))
    (values instance)))

(defgeneric list-resource-identifiers (steward resource-class)
  (:documentation
   "List the identifiers for resources of RESOURCE-CLASS known by STEWARD.
These resources can be imported."))

(defun list-resources (steward resource-class)
  "List resources of RESOURCE-CLASS known by STEWARD."
  (loop :for identifier :in (list-resource-identifiers steward resource-class)
	:collect (import-resource
		  :steward steward
		  :identifier identifier
		  :pathname identifier)))

;;;; End of file `resource.lisp'
