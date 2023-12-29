;;;; resource.lisp — Resources for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
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
    :db-constraints :not-null)
   (project-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null)
   (steward-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :initarg :pathname
    :reader steward-pathname)
   (steward-class
    :type symbol
    :allocation :class)
   (pathname
    :initarg :pathname
    :initform (error "A RESOURCE requires a PATHNAME.")
    :documentation "A name for the RESOURCE.
The fully qualified name of the RESOURCE, which should be a safe Unix path. This PATHNAME
uniquely identifies the RESOURCE within the deployment it belongs to.")
   (description
    :initarg :description
    :documentation "A short description of the RESOURCE.")
   (steward
    :initarg :steward
    :initform (error "A RESOURCE requires a PROVIDER.")
    :documentation "The provider this RESOURCE belongs to.")
   (identification
    :initarg :identification
    :initform nil
    :documentation
    "A text uniquely identifying the RESOURCE in the context of its STEWARD.
Depending on the RESOURCE and the STEWARD, this text can sometimes
not be determined automatically and requires the RESOURCE to be READ."))
  (:documentation "The class represents the state of resources required to a component deployment.
These resources can be created, read, updated and deleted. Resources can depend on other
resources. For a given STEWARD, any resource is uniquely identified by its IDENTIFICATION slot."))


(defmethod print-object ((instance resource) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'tenant-pathname)
	       (slot-boundp instance 'project-pathname)
	       (slot-boundp instance 'steward-pathname)
	       (slot-boundp instance 'pathname)
	       (slot-boundp instance 'identification))
      (with-slots (tenant-pathname project-pathname steward-pathname
		   pathname identification)
	  instance
	(format stream "~A ~A ~A ~A ~A"
		tenant-pathname project-pathname steward-pathname
		pathname (or identification "N/A"))))))

(defmethod describe-object ((instance resource) stream)
  (with-slots (description steward) instance
    (format stream "~&~A is a resource of type ~A provided by a ~A."
	    instance (type-of instance) (type-of steward))
    (when description
      (format stream "~&Description: ~A" description))
    (format stream "~&Steward: ~A" steward))
  (values))

(defmethod initialize-instance :after ((instance resource)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((check-that-steward-and-steward-class-match ()
	   (with-slots (steward steward-class) instance
	     (unless (typep steward steward-class)
	       (error "Steward mismatch.
A resource of type ~A requires a ~A steward. However, the steward used
to initialise this resource was a ~A."
		      (type-of instance) steward-class (type-of steward))))))
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
      (warn "Cannot create the resource ~A (~A) as it already exists." pathname identifier)
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

(defgeneric import-resource (&key steward pathname description identifier)
  (:documentation
   "Import a RESOURCE based on its IDENTIFIER into its STEWARD.
This assumes a resource has been created in STEWARD by a third party and
imports it into the current system by creating a resource for it."))

(defmethod import-resource :around (&key steward pathname description identifier)
  (let ((instance
	  (call-next-method)))
    (macrolet ((copy-slot-value (slot-name)
		 `(setf (slot-value instance ',slot-name) ,slot-name)))
      (copy-slot-value pathname)
      (copy-slot-value description)
      (copy-slot-value identifier)
      (copy-slot-value steward))
    (values instance)))

;;;; End of file `resource.lisp'
