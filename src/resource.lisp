;;;; resource.lisp — Resources for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
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

(defclass resource nil
  ((pathname
    :initarg :pathname
    :initform (error "A RESOURCE requires a PATHNAME.")
    :documentation "A name for the RESOURCE.
The fully qualified name of the RESOURCE, which should be a safe Unix path. This PATHNAME
uniquely identifies the RESOURCE within the deployment it belongs to.")
   (description
    :initarg :description
    :documentation "A short description of the RESOURCE.")
   (provider
    :initarg :provider
    :initform (error "A RESOURCE requires a PROVIDER.")
    :documentation "The provider this RESOURCE belongs to.")
   (provider-class
    :initform (error "A RESOURCE requires a PROVIDER-CLASS.")
    :documentation "The class a provider for this resource must belong to.")
   (identification
    :initarg :identification
    :initform nil
    :documentation
    "A text uniquely identifying the RESOURCE in the context of its PROVIDER.
Depending on the RESOURCE and the PROVIDER, this text cannot be determined a priori and
requires the RESOURCE to be READ."))
  (:documentation "The class represents the state of resources required to a component deployment.
These resources can be created, read, updated and deleted. Resources can depend on other
resources. For a given PROVIDER, any resource is uniquely identified by its IDENTIFICATION slot."))

(defmethod print-object ((instance resource) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S" (if (slot-boundp instance 'pathname)
			 (slot-value instance 'pathname)
			 "(no pathname)"))))

(defmethod describe-object ((instance resource) stream)
  (with-slots (description provider) instance
    (format stream "~&~A is a resource of type ~A provided by a ~A."
	    instance (type-of instance) (type-of provider))
    (when description
      (format stream "~&Description: ~A" description))
    (format stream "~&Provider: ~A" provider))
  (values))

(defmethod initialize-instance :after ((instance resource)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (provider provider-class) instance
    (unless (typep provider provider-class)
      (error "Provider mismatch.
A resource of type ~A requires a ~A provider. However, the provider used
to initialise this resource was a ~A."
	     (type-of instance) provider-class (type-of provider))))
  (values))



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
error DESCRIPTIONS used by the same provider.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer explanation describing the RESOURCE-ERRROR.
This longer EXPLANATION is a text that could be presented to the console
operator.  It is allowed to feature details specific to the operation or
the resource, so that it is usually unsafe to publish this EXPLANATION."))
  (:report
   (lambda (condition stream)
     (with-slots (pathname provider) (resource-error-resource condition)
       (let ((*print-circle* nil))
	 (format stream "~&Operation on resource ~A failed.

The provider ~A trying to ~A the resource ~A met an error condition.
~A" 
		 pathname
		 (slot-value (find-provider provider) 'pathname)
		 (resource-error-operation condition) pathname
		 (resource-error-description condition))
	 (with-slots (explanation) condition
	   (when explanation
	     (format stream "~&~A" explanation)))))))
  (:documentation
   "This condition is signaled when a provider operating a resource meets an error condition."))

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
confirmation DESCRIPTIONS used by the same provider.")
   (explanation
    :initarg :explanation
    :initform nil
    :documentation "A longer explanation describing the RESOURCE-ERRROR.
This longer EXPLANATION is a text that could be presented to the console
operator.  It is allowed to feature details specific to the operation or
the resource, so that it is usually unsafe to publish this EXPLANATION."))
  (:report
   (lambda (condition stream)
     (with-slots (pathname provider) (resource-confirmation-resource condition)
       (let ((*print-circle* nil))
	 (format stream "Operation on resource ~A requires confirmation.

The provider ~A is trying to ~A the resource ~A and requires confirmation to proceed.
~A" 
		 pathname
		 (slot-value (find-provider provider) 'pathname)
		 (resource-confirmation-operation condition) pathname
		 (resource-confirmation-description condition))
	 (with-slots (explanation) condition
	   (when explanation
	     (format stream "~&~A" explanation)))))))
  (:documentation
   "This condition is signaled when a provider operating a resource requires confirmation."))

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

(defgeneric actually-create-resource (resource)
  (:documentation "Create a RESOURCE using its provider.
The ACTUALLY-CREATE-RESOURCE generic function is the customisable part of
the logic for `CREATE-RESOURCE'.")
  (:method (resource)
    (declare (ignore resource))
    (error "Nothing is known about how to create this resource.")))

(defun create-resource (resource)
  "Create a RESOURCE using its provider."
  (with-slots (pathname identification) resource
    (when identification
      (warn "create-resource: ~A (~A): This resource already exists." pathname identification)
      (return-from create-resource)))
  (actually-create-resource resource)
  resource)

(defgeneric actually-read-resource (resource)
  (:documentation "Read a RESOURCE using its provider.
If the resource can be found in the context of its PROVIDER based on its IDENTIFICATION,
then RESOURCE slots are updated and the RESOURCE is returned.

If the resource cannot be found in the context of its PROVIDER based on its IDENTIFICATION,
then the IDENTIFICATION is wiped and NIL is returned.")
  (:method (resource)
    (declare (ignore resource))
    (error "Nothing is known about how to read this resource.")))

(defun read-resource (resource)
  "Read a RESOURCE using its provider.
If the resource can be found in the context of its PROVIDER based on its IDENTIFICATION,
then RESOURCE slots are updated and the RESOURCE is returned.

If the resource cannot be found in the context of its PROVIDER based on its IDENTIFICATION,
then the IDENTIFICATION is wiped and NIL is returned.

If the resource has no IDENTIFICATION then NIL is returned."
  (with-slots (identification) resource
    (unless identification
      (return-from read-resource nil))
    (actually-read-resource resource)
    resource))

(defgeneric actually-delete-resource (resource)
  (:documentation "Delete a RESOURCE using its provider.")
  (:method (resource)
    (declare (ignore resource))
    (error "Nothing is known about how to delete this resource.")))

(defun delete-resource (resource)
  "Delete a RESOURCE using its provider.
If the RESOURCE does not exist, NIL is returned otherwise the RESOURCE is returned."
  (with-slots (identification) resource
    (unless identification
      (return-from delete-resource nil))
    (actually-delete-resource resource)
    (setf identification nil)
    resource))

(defgeneric actually-update-resource (resource)
  (:documentation "Update a RESOURCE using its provider.")
  (:method (resource)
    (with-slots (pathname identification) resource
      (resource-confirmation :update resource
			     "There is no specific knowledge on how to update ~A (~A).
We therefore need to delete it and create a new resource to replace it. The
resource will therefore be unavailable and could cause service disruption."
			     pathname identification))
    (delete-resource resource)
    (create-resource resource)))

(defun update-resource (resource)
  "Update a RESOURCE using its provider.
If the RESOURCE does not exist, NIL is returned."
  (with-slots (identification) resource
    (unless identification
      (return-from update-resource nil))
    (actually-update-resource resource)
    resource))

(defgeneric actually-examine-resource (resource)
  (:documentation "Dump the state of a RESOURCE.
The result is a property list, mapping keywords to atoms.")
  (:method-combination append)
  (:method append (resource)
    (with-slots (pathname identification description provider) resource
      (list
       :pathname pathname
       :identification identification
       :description description
       :provider (slot-value provider 'pathname)))))

(defun examine-resource (resource)
  "The propeties of a RESOURCE.
The state is a property list whose key is a KEYWORD and value an ATOM or a LIST. These
keywords are sorted in ascending order."
  (sort-plist
   (actually-examine-resource resource)))

(defgeneric actually-import-resource (provider identifier)
  (:documentation "Create a RESOURCE based on existing resource IDENTIFIER within PROVIDER.
If such a resource does not exist, an RESOURCE-ERROR condition is reported.")
  (:method (provider identifier)
    (declare (ignore provider identifier))
    (error "Nothing is known about how to import this resource.")))

(defun import-resource (&key provider pathname description identification)
  "Import a RESOURCE into its PROVIDER.
This assumes a resource has been created in PROVIDER by a third party and
imports it into the current system."
  (let ((resource
	  (actually-import-resource provider identification)))
    (setf (slot-value resource 'pathname) pathname
	  (slot-value resource 'description) description)
    resource))


;;;;
;;;; Computing Resources Methods
;;;;

(defclass computing-resource (resource)
  ((status
    :initarg :status
    :initform :halt
    :documentation "The status of a computing RESOURCE.
The possible values for the status are:

  :STOPPED
    The computing resource has been stopped and is not running.

  :RUNNING
    The computing resource has been started and is running.

  :STARTING
    The computing resource is transitioning from the :STOPPED state to
    the :RUNNING state but is not yet available.

  :SHUTTING-DOWN
    The computing resource is transtioning from the :RUNNING to
    the :STOPPED state."))
  (:documentation "The class represents computing resources.
These resources can be turned on and off."))

(defgeneric actually-start-resource (resource)
  (:documentation "Start a RESOURCE using its provider.
The ACTUALLY-START-RESOURCE generic function is the customisable part of
the logic for `START-RESOURCE'.")
  (:method ((instance resource))
    (declare (ignore instance)))
  (:method ((instance computing-resource))
    (declare (ignore instance))
    (error "Nothing is known about how to start this resource.")))

(defun start-resource (resource)
  "Start RESOURCE."
  (with-slots (status) resource
    (ecase status
      ((:starting :running)
       t)
      (:shutting-down
       (error "Cannot start a shutting-down resource."))
      (:stopped
       (setf status :starting)
       (actually-start-resource resource)
       (setf status :running)))))

(defgeneric actually-stop-resource (resource)
  (:documentation "Stop a RESOURCE using its provider.
The ACTUALLY-STOP-RESOURCE generic function is the customisable part of
the logic for `STOP-RESOURCE'.")
  (:method ((instance resource))
    (declare (ignore instance)))
  (:method ((instance computing-resource))
    (declare (ignore instance))
    (error "Nothing is known about how to stop this resource.")))

(defun stop-resource (resource)
  "Stop RESOURCE."
  (with-slots (status) resource
    (ecase status
      ((:stopped :shutting-down)
       t)
      (:starting
       (error "Cannot shut-down a starting resource."))
      (:running
       (setf status :shutting-down)
       (actually-stop-resource resource)
       (setf status :stopped)))))


;;;;
;;;; Stateful Resources Methods
;;;;

(defclass stateful-resource (resource)
  nil
  (:documentation "The class represents stateful resources.
The state of one of these resource can be dumped into a file and later be restored."))

(defgeneric actually-dump-resource (resource)
  (:documentation "Dump a RESOURCE using its provider.
The ACTUALLY-DUMP-RESOURCE generic function is the customisable part of
the logic for `DUMP-RESOURCE'.")
  (:method ((instance resource))
    (declare (ignore instance)))
  (:method ((instance stateful-resource))
    (declare (ignore instance))
    (error "Nothing is known about how to dump this resource.")))

(defun dump-resource (resource)
  "Dump RESOURCE."
  (actually-dump-resource resource))

(defgeneric actually-restore-resource (resource)
  (:documentation "Restore a RESOURCE using its provider.
The ACTUALLY-RESTORE-RESOURCE generic function is the customisable part of
the logic for `RESTORE-RESOURCE'.")
  (:method ((instance resource))
    (declare (ignore instance)))
  (:method ((instance stateful-resource))
    (declare (ignore instance))
    (error "Nothing is known about how to restore this resource.")))

(defun restore-resource (resource)
  "Restore RESOURCE."
  (actually-restore-resource resource))

;;;; End of file `resource.lisp'
