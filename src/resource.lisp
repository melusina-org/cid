;;;; resource.lisp — Resources for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

;;;;
;;;; Resource Class
;;;;

(defclass resource ()
  ((steward
    :type (or steward null)
    :reader steward
    :initarg :steward
    :documentation "The steward of a resource.
When a *STEWARD-DIRECTORY* is active, keywords can be used to initialise this slot.")
   (steward-class
    :type symbol
    :allocation :class)
   (name
    :accessor name
    :type (or string null)
    :initform nil
    :initarg :name
    :documentation "The NAME is used in technical context to denote the resource instance.")
   (displayname
    :accessor displayname
    :type (or string null)
    :initform nil
    :initarg :displayname
    :documentation "The DISPLAYNAME is used in informational screens to denote the resource instance.")
   (description
    :type (or string null)
    :initarg :description
    :reader description
    :initform nil
    :documentation "A short description of the RESOURCE.")
   (state
    :type symbol
    :initarg :state
    :initform nil
    :documentation "The state of the resource.
Allowed values are NIL, meaning the resource is not ready or maybe does not exist yet,
T meaning the resource exists and is ready or some resource lifecycle specific keyword.")
   (identifier
    :type (or string null)
    :initarg :identifier
    :reader resource-identifier
    :initform nil
    :documentation
    "A text uniquely identifying the RESOURCE in the context of its STEWARD.
Depending on the RESOURCE and the STEWARD, the identifier can be or not be
a deterministic function of other resource properties. If and only if
the underlying resource has not been created, the IDENTIFIER is NIL.")
   (parent
    :initarg :parent
    :reader resource-parent
    :initform nil
    :documentation
    "A parent resource.")
   (external
    :type boolean
    :initarg :external
    :reader resource-external-p
    :initform nil
    :documentation
    "A flag marking external resources.
Unlike internal resources which are owned by the program operating on them,
external resources have a distinct owner.  External resources must not be
altered by the program operating on them."))
  (:documentation "The class represents the state of resources required to a component deployment.
These resources can be created, read, updated and deleted. Resources can depend on other
resources. For a given STEWARD, any resource is uniquely identified by its IDENTIFIER slot."))

(defmethod initialize-instance :after ((instance resource) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((support-initialize-steward-slot-with-keyword ()
	   (with-slots ((designator steward)) instance
	     (when (and *steward-directory* (keywordp designator))
	       (let ((actual-steward
		       (find-steward designator)))
		 (unless actual-steward
		   (error "There is no steward identified by ~A." designator))
		 (setf designator actual-steward))))))
    (support-initialize-steward-slot-with-keyword)))

(defmethod tenant ((instance resource))
  (tenant (steward instance)))

(defmethod project ((instance resource))
  (project (steward instance)))

(defmethod persistent-slots append ((instance resource))
  '((:initarg :name
     :slot-name name)
    (:initarg :displayname
     :slot-name displayname)
    (:initarg :description
     :slot-name description)
    (:initarg :state
     :slot-name state)
    (:initarg :identifier
     :slot-name identifier)
    (:initarg :parent
     :slot-name parent)))

(defmethod print-object ((instance resource) stream)
  (flet ((print-readably ()
	   (write-persistent-object instance stream))
	 (print-unreadably ()
	   (flet ((print-scope ()
		    (format stream "~A:~A"
			    (name (tenant instance))
			    (name (project instance))))
		  (print-name ()
		    (with-slots (name displayname) instance
		      (when (and name displayname)
			(format stream ":~A ~A" name displayname))))
		  (print-state-and-identifier ()
		    (with-slots (state identifier) instance
		      (when identifier
			(format stream " ~A" identifier))
		      (format stream " :STATE ~A" state))))
	     (print-unreadable-object (instance stream :type t :identity t)
	       (print-scope)
	       (print-name)
	       (print-state-and-identifier)))))
    (if *print-readably*
	(print-readably)
	(print-unreadably))))


;;;;
;;;; Resource Error
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
  (:report describe-resource-error)
  (:documentation
   "This condition is signaled when a steward operating a resource meets an error condition."))

(defun describe-resource-error (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle*
	    nil)
	  (effective-displayname
	    (or displayname name (resource-error-resource condition))))
      (format stream "~A" (resource-error-description condition))
      (start-new-paragraph stream)
      (format stream "~@<The steward ~A trying to ~A the resource ~A met an error condition.~:@>"
	      (slot-value steward 'name)
	      (case (resource-error-operation condition)
		(create-resource "create")
		(delete-resource "delete")
		(import-resource "import")
		(t
		 (resource-error-operation condition)))
	      effective-displayname)
      (with-slots (explanation) condition
	(when explanation
	  (start-new-paragraph stream)
 	  (format stream "~A" explanation))))))

(defun resource-error (operation resource description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-ERROR."
  (error 'resource-error
	 :operation operation
	 :resource resource
	 :description description
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))


;;;;
;;;; Resource no Longer Exists
;;;;

(define-condition resource-no-longer-exists (resource-error)
  nil
  (:report describe-resource-no-longer-exists)
  (:documentation
   "This condition is signaled when a steward operating a resource realises
that the underlying resource no longer exists while the last state known
to the resource handle indicates that the underlying resource exists."))

(defun describe-resource-no-longer-exists (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle*
	    nil)
	  (effective-displayname
	    (or displayname name (resource-error-resource condition))))
      (format stream "~&Resource ~A no longer exists." effective-displayname)
      (start-new-paragraph stream)
      (format stream "~@<The steward ~A trying to ~A the resource ~A realised that the underlying resource ~A no longer exists while the last known state indicates the underlying resource was existing.~:@>"
	      (slot-value steward 'name)
	      (case (resource-error-operation condition)
		(create-resource "create")
		(delete-resource "delete")
		(import-resource "import")
		(t
		 (resource-error-operation condition)))
	      effective-displayname
	      effective-displayname)
      (with-slots (explanation) condition
	(when explanation
	  (start-new-paragraph stream)
	  (format stream "~A" explanation))))))

(defun resource-no-longer-exists (operation resource description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-NO-LONGER-EXISTS."
  (error 'resource-no-longer-exists
	 :operation operation
	 :resource resource
	 :description description
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))


;;;;
;;;; Resource Slot is Immutable
;;;;

(define-condition resource-slot-is-immutable (resource-error)
  ((slot-name
    :type symbol
    :initarg :slot-name
    :reader resource-error-slot-name))
  (:report describe-resource-slot-is-immutable)
  (:documentation
   "This condition is signaled when we try to update
an immutable slot of a RESOURCE using the UPDATE-RESOURCE-FROM-INSTANCE
generic function."))

(defun describe-resource-slot-is-immutable (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle* nil))
      (format stream "~&Operation on resource ~A failed.

The slot ~A of resource ~A is immutable and cannot be updated." 
	      (or displayname name (resource-error-resource condition))
	      (resource-error-slot-name condition)
	      (or displayname name (resource-error-resource condition)))
      (with-slots (explanation) condition
	(when explanation
	  (format stream "~&~A" explanation))))))

(defun resource-slot-is-immutable (operation resource description slot-name &optional control-string &rest format-arguments)
  "Signal a RESOURCE-SLOT-IS-IMMUTABLE."
  (error 'resource-slot-is-immutable
	 :operation operation
	 :resource resource
	 :description description
	 :slot-name slot-name
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))


;;;;
;;;; Resource is External
;;;;

(define-condition resource-is-external (resource-error)
  nil
  (:report describe-resource-is-external)
  (:documentation
   "This condition is signaled when we try to update
an extenal resource in one of the generic functions CREATE-RESOURCE,
UPDATE-RESOURCE-FROM-INSTANCE or DELETE-RESOURCE."))

(defun describe-resource-is-external (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle* nil))
      (format stream "~&Operation on resource ~A failed.

The resource ~A is external and must not be altered." 
	      (or displayname name (resource-error-resource condition))
	      (or displayname name (resource-error-resource condition)))
      (with-slots (explanation) condition
	(when explanation
	  (format stream "~&~A" explanation))))))

(defun resource-is-external (operation resource description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-IS-EXTERNAL condition."
  (error 'resource-is-external
	 :operation operation
	 :resource resource
	 :description description
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))



;;;;
;;;; Resource Already Exists
;;;;

(define-condition resource-already-exists (resource-error)
  nil
  (:report describe-resource-already-exists)
  (:documentation
   "This condition is signaled when a steward operating a resource realises
that the underlying resource already exists while the last state known
to the resource handle indicates that the underlying resource did not exist."))

(defun describe-resource-already-exists (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle* nil))
      (format stream "~&Operation on resource ~A failed.

The steward ~A trying to ~A the resource ~A realised that
the underlying resource ~A already exists while the last
known state indicates the underlying resource did exist." 
	      (or displayname name (resource-error-resource condition))
	      (slot-value steward 'name)
	      (resource-error-operation condition)
	      (or displayname name (resource-error-resource condition))
	      (resource-error-description condition))
      (with-slots (explanation) condition
	(when explanation
	  (format stream "~&~A" explanation))))))

(defun resource-already-exists (operation resource description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-ALREADY-EXISTS."
  (error 'resource-already-exists
	 :operation operation
	 :resource resource
	 :description description
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))


;;;;
;;;; Resource Prerequisite is Missing
;;;;

(define-condition resource-prerequisite-is-missing (resource-error)
  ((prerequisite
    :type resource
    :initarg :prerequisite
    :reader resource-error-prerequisite))
  (:report describe-resource-prerequisite-is-missing)
  (:documentation
   "This condition is signaled when a steward creating a resource realises
that the prerequisites of the resource do not yet exist."))

(defun describe-resource-prerequisite-is-missing (condition stream)
  (with-slots (name displayname steward) (resource-error-resource condition)
    (let ((*print-circle* nil))
      (format stream "~&Operation on resource ~A failed.

The steward ~A trying to ~A the resource ~A realised that
some prerequisite ~A for that resource do not yet exist." 
	      (or displayname name (resource-error-resource condition))
	      (slot-value steward 'name)
	      (resource-error-operation condition)
	      (or displayname name (resource-error-resource condition))
	      (resource-error-prerequisite condition))
      (with-slots (explanation) condition
	(when explanation
	  (format stream "~&~A" explanation))))))

(defun resource-prerequisite-is-missing (operation resource prerequisite description &optional control-string &rest format-arguments)
  "Signal a RESOURCE-PREREQUISITE-IS-MISSING."
  (error 'resource-prerequisite-is-missing
	 :operation operation
	 :resource resource
	 :prerequisite prerequisite
	 :description description
	 :explanation (when control-string
			(apply #'format nil control-string format-arguments))))


;;;;
;;;; Resource Confirmation
;;;;

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
     (with-slots (name steward) (resource-confirmation-resource condition)
       (let ((*print-circle* nil))
	 (format stream "Operation on resource ~A requires confirmation.

The steward ~A is trying to ~A the resource ~A and requires confirmation to proceed.
~A" 
		 name
		 (slot-value steward 'name)
		 (resource-confirmation-operation condition) name
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
;;;; Resources with Prerequisites
;;;;

(defgeneric resource-prerequisites (resource)
  (:method-combination append)
  (:documentation
   "List resources that need to be created before RESOURCES can be.
Some resources require other resources to exist before being created
themselves. The RESOURCE-PREREQUISITES generic function lists these
prerequisites in an order so that any resource in the list appears
before its prerequisites."))

(defmethod resource-prerequisites append ((instance resource))
  (when (slot-value instance 'parent)
    (list (slot-value instance 'parent))))

(defmethod resource-prerequisites :around ((instance resource))
  "Ensure the prerequisites of a resource are sorted."
  (loop :for prerequisite :in (call-next-method)
	:for deep-prerequisites = (resource-prerequisites prerequisite)
	:append (cons prerequisite deep-prerequisites) :into prerequisites
	:finally (return (remove-duplicates prerequisites :test #'eq))))

(defun resource-prerequisite-p (resource1 resource2)
  "Predicate recognising when RESOURCE1 is a prerequisite of RESOURCE2."
  (and (member resource1 (resource-prerequisites resource2)) t))

(defun resource-require-p (resource1 resource2)
  "Predicate recognising when RESOURCE1 requires RESOURCE2.
This is equivalent to say that RESOURCE2 is a prerequisite of RESOURCE1."
  (and (member resource2 (resource-prerequisites resource1)) t))


(defun sort-resources (resources)
  "Sort RESOURCES so that any resource in the list appears before its prerequisites."
  (sort resources #'resource-require-p))
  

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

(defgeneric resource-internal-p (resource)
  (:documentation "Predicate recognising RESOURCES that are internal.")
  (:method ((instance resource))
    (not (resource-external-p instance))))

(defgeneric create-resource (resource)
  (:documentation "Create a RESOURCE using its steward.
This returns the RESOURCE instance."))

(defmethod create-resource :around ((instance resource))
  "Ensure that we do not recreate a resource that already exists.
When we try to recreate a resource that already exists, we signal
a continuable error.

This also enforces the calling convention, ensuring that the generic
function returns only one value, the instance created."
  (with-slots (identifier state) instance
    (when state
      (restart-case
	  (resource-already-exists
	   'create-resource instance
	   "Cannot create the resource as it already exists."
	   "The resource ~A cannot be created as the underlying resource
already exists."
	   instance)
	(use-resource ()
	  :report "Use the existing resource."
	  (return-from create-resource instance))
	(recreate-resource ()
	  :report "Delete and recreate the resource."
	  (delete-resource instance)))))
  (call-next-method)
  (values instance))

(defmethod create-resource :before ((instance resource))
  "Verify INSTANCE prerequisites do exist.
When a prerequisite does not exist, an error is signaled
in a context where a CREATE-PREREQUISITE restart is available."
  (flet ((ensure-that-resource-is-internal (resource)
	   (unless (resource-internal-p resource)
	     (resource-is-external
	      'create-resource resource
	      "External resources cannot be altered."
	      "The resource ~A is marked as external and it must not be altered.
In particuar, this resource must not be created."
	      resource)))
	 (ensure-that-prerequisite-exists (prerequisite)
	   (unless (resource-exists-p prerequisite)
	     (restart-case
		 (resource-prerequisite-is-missing
		  'create-resource
		   instance prerequisite
		   "Resource prerequisite does not exist.")
	       (create-missing-prerequisite ()
		 :report "Create the missing prerequisite."
		 (create-resource prerequisite))))))
    (ensure-that-resource-is-internal instance)
    (loop :for prerequisite :in (reverse (resource-prerequisites instance))
	  :do (ensure-that-prerequisite-exists prerequisite))))

(defun create-missing-prerequisite (c)
  "A restart function invoking the CREATE-MISSING-PREREQUISITE restart.
This restart function is useful in a HANDLER-BIND clause around an
invokation of the CREATE-RESOURCE function or the
APPLY-MODIFICATION-INSTRUCTIONS function."
  (declare (ignore c))
  (let ((restart (find-restart 'create-missing-prerequisite)))
    (when restart (invoke-restart restart))))

(defgeneric delete-resource (resource)
  (:documentation "Delete a RESOURCE using its steward.
This returns the RESOURCE instance."))

(defmethod delete-resource :before ((instance resource))
  "Verify INSTANCE is internal before deleting it."
  (flet ((ensure-that-resource-is-internal (resource)
	   (unless (resource-internal-p resource)
	     (resource-is-external
	      'delete-resource resource
	      "External resources cannot be altered."
	      "The resource ~A is marked as external and it must not be altered.
In particuar, this resource must not be deleted."
	      resource))))
    (ensure-that-resource-is-internal instance)))

(defmethod delete-resource :around ((instance resource))
  "Enforce calling convention and ensure that we do not delete a resource that does not exist.
When a RESOURCE-NO-LONGER-EXISTS condition is met, a CONTINUE restart is made available allowing
to assume the delete operation was succesful."
  (with-slots (identifier state) instance
    (unless state
      (warn "Cannot delete the resource ~A as it does not exist." identifier)
      (return-from delete-resource instance))
    (restart-case (call-next-method)
      (continue ()
	:report
	"Ignore no longer existing resource and assume the delete operation was successful."
	:test
	(lambda (condition)
	  (and (typep condition 'resource-no-longer-exists)
	       (eq (resource-error-operation condition) 'delete-resource)))
	(values)))
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
  (flet ((check-that-resource-has-an-identifier ()
	   (unless (slot-value instance 'identifier)
	     (resource-error
	      'update-instance-from-resource instance
	      "Cannot update instance from resource without a resource identifier."
	      "The resource instance ~A has no resource identifier attached
it is therefore impossible to examine the resource attached to this identifier."
	      instance)))
	 (call-next-method-with-restartable-resource-no-longer-exists ()
	   (handler-case (call-next-method)
	     (resource-no-longer-exists (condition)
	       (declare (ignore condition))
	       (with-slots (state identifier) instance
		 (setf state nil
		       identifier nil))))))
    (check-that-resource-has-an-identifier)
    (call-next-method-with-restartable-resource-no-longer-exists)
    (values instance)))

(defgeneric examine-resource (resource)
  (:method-combination append)
  (:documentation "Dump the state of a RESOURCE.
The result is a property list, mapping keywords to atoms. These
keywords are sorted in ascending order."))

(defmethod examine-resource :around ((instance resource))
  "Ensure the results of examining a resource are sorted."
  (sort-plist (call-next-method)))

(defmethod examine-resource append ((instance resource))
  (with-slots (steward displayname identifier description state) instance
    (list
     :steward (name steward)
     :displayname displayname
     :identifier identifier
     :description description
     :state state)))

(defun import-resource (steward resource-class &key displayname description identifier parent)
  "Import a RESOURCE based on its IDENTIFIER into its STEWARD.
This assumes a resource has been created in STEWARD by a third party and
imports it into the current system by creating a resource for it."
  (let ((instance
	  (make-instance resource-class
			 :steward steward
			 :displayname displayname
			 :description description
			 :identifier identifier
			 :parent parent)))
    (update-instance-from-resource instance)
    (unless (resource-exists-p instance)
      (resource-error
       'import-resource
       instance
       "Resource not found"
       "The resource ~S was not found by steward ~A and it is impossible to import it."
       identifier steward))
    (values instance)))

(defun actual-resource (resource)
  "Import the RESOURCE using its steward.
The resulting resource reflects the actual state of the underlying resource
instead of the expected state as specified by RESOURCE."
  (import-resource (steward resource) (type-of resource)
		   :displayname (displayname resource)
		   :description (description resource)
		   :identifier (resource-identifier resource)
		   :parent (resource-parent resource)))

(defgeneric update-resource-from-instance (instance)
  (:documentation "Update resource attributes to reflect INSTANCE.
The attributes of the underlying resource are examined and when differing
from the attributes of INSTANCE, the resource is modified to reflect
the state of instance.

It is an error to update an immutable slot using this function. It is
an error to update the STATE or the IDENTIFIER slot using this function.

It is an error to update a resource that does not exist. Use the CREATE-RESOURCE
function to do so. While the CREATE-RESOURCE function could be called as
a restart to recover from the error, such an error probably result from
a programming logic and must be understood as a hint of a larger problem."))

(defmethod update-resource-from-instance :before ((instance resource))
  "Signal errors when updating not created resources."
  (flet ((ensure-that-resource-is-internal (resource)
	   (unless (resource-internal-p resource)
	     (resource-is-external
	      'update-resource-from-instance resource
	      "External resources cannot be altered."
	      "The resource ~A is marked as external and it must not be altered.
In particuar, this resource must not be updated."
	      resource)))
	 (signal-error-when-updating-not-created-resource ()
	   (unless (resource-exists-p instance)
	     (resource-error
	      'update-resource-from-instance
	      instance
	      "Cannot update resource before it has been created."
	      "The resource instance ~A has no resource identifier attached
it is therefore impossible to update the resource attached to this identifier."
	      instance)))
	 (signal-error-when-updating-immutable-slots ()
	   (let ((actual
		   (actual-resource instance))
		 (slots-spec
		   (persistent-slots instance)))
	     (flet ((signal-on-update-immutable (spec)
		      (unless (getf spec :immutable)
			(return-from signal-on-update-immutable))
		      (let ((slot-name (getf spec :slot-name)))
			(unless (equal (slot-value instance slot-name)
				       (slot-value actual slot-name))
			  (resource-slot-is-immutable
			   'update-resource-from-instance
			   instance
			   "Cannot update immutable slot of a resource."
			   slot-name
			   "Immutable slot ~A of the resource instance ~A cannot be modified. Instead the underlying resource must be deleted and recreated."
			   slot-name instance)))))
	       (mapc #'signal-on-update-immutable slots-spec)))))
    (ensure-that-resource-is-internal instance)
    (signal-error-when-updating-not-created-resource)
    (signal-error-when-updating-immutable-slots)))

(defmethod update-resource-from-instance :around ((instance resource))
  "Enforce calling convention."
  (call-next-method)
  (values instance))

(defgeneric list-resource-identifiers (steward resource-class)
  (:documentation
   "List the identifiers for resources of RESOURCE-CLASS known by STEWARD.
These resources can be imported."))

(defgeneric list-resources (steward resource-class)
  (:documentation
   "List resources of RESOURCE-CLASS known by STEWARD."))

(defmethod list-resources (steward resource-class)
  (loop :for identifier :in (list-resource-identifiers steward resource-class)
	:collect (import-resource
		  steward
		  resource-class
		  :identifier identifier)))


;;;;
;;;; Update Resources from a Blueprint
;;;;

(defun apply-modification-instructions (instructions)
  "Process resource modifying INSTRUCTIONS.
The possible INSTRUCTIONS and their semantics are described below:

  :CREATE-RESOURCE RESOURCE
    Create the given RESOURCE.

  :DELETE-RESOURCE RESOURCE
    Delete the given RESOURCE.

  :UPDATE-INSTANCE RESOURCE {SLOT-NAME SLOT-VALUE}*
    Update the RESOURCE instance so that its slots take the specified values.

  :UPDATE-RESOURCE RESOURCE
    Update the underlying RESOURCE so that it reflects the change carried on the
    INSTANCE."
  (flet ((apply-update-instance (instance slot-names-and-values)
	   (loop :for (slot-name slot-value other-slots)
		 :on slot-names-and-values :by #'cddr
		 :do (setf (slot-value instance slot-name) slot-value)))
	 (apply-update-resource (instance)
	   (update-resource-from-instance instance))
	 (apply-create-resource (instance)
	   (create-resource instance))
	 (apply-delete-resource (instance)
	   (delete-resource instance)))
    (loop :for instruction :in instructions
	  :do
	  (ecase (first instruction)
	    (:update-instance
	     (apply-update-instance (second instruction) (cddr instruction)))
	    (:update-resource
	     (apply-update-resource (second instruction)))
	    (:create-resource
	     (apply-create-resource (second instruction)))
	    (:delete-resource
	     (apply-delete-resource (second instruction)))))))

(defun prepare-modification-instructions (resource blueprint)
  "Prepare INSTRUCTIONS to modify RESOURCE to resemble the BLUEPRINT.
When applied the instruction must update the RESOURCE instance so that
its slots take the values of the slots of BLUEPRINT.

The BLUEPRINT must have the same type as the RESOURCE, and
each prerequisite of the BLUEPRINT must have the same type
as the corresponding prerequisite of the RESOURCE. Furthermore,
physically equal prerequisites for RESOURCE must also have
physically equal counterparts in BLUEPRINT.

Some slots are handled specially by the process, such as the STATE
and the IDENTIFIER. The STATE and IDENTIFIER slots from the BLUEPRINT,
which are ignored.

Slots which are marked as resource prerequisites are handled differently
wether the prerequisite can be updated or requires to be recreated.

The resulting instructions follow the consistency rules below:

 1. The only resources affected by the instructions are the RESOURCE
    and its prerequisites.

 2. Any affected resource is deleted at most once.

 3. Any affected resource which is deleted is deleted before
    every of its deleted prerequisites.

 4. Any affected resource is created at most once.

 5. Any affected resource which is created is created after
    every of its created prerequisites is created."
  (labels ((type-equal-p (object1 object2)
	     (and (subtypep (type-of object1) (type-of object2))
		  (subtypep (type-of object2) (type-of object1))))
	   (check-that-resource-and-blueprint-have-the-same-type ()
	     (unless (type-equal-p resource blueprint)
	       (error "The RESOURCE and its modification BLUEPRINT have distinct types.")))
	   (slots-spec (blueprint)
	     (flet ((ignored-slot-p (slot-name)
		      (member slot-name '(state identifier)))
		    (getf-slot-name (spec)
		      (getf spec :slot-name)))
	       (remove-if #'ignored-slot-p 
			  (persistent-slots blueprint)
			  :key #'getf-slot-name)))
	   (affected-resources (resource)
	     (cons resource (resource-prerequisites resource)))
	   (make-strategy (resource blueprint)
	     (let ((affected-resources
		     (affected-resources resource))
		   (affected-blueprint
		     (affected-resources blueprint)))
	       (unless (eq (length affected-resources) (length affected-blueprint))
		 (error "Prerequisites for RESOURCE and its BLUEPRINT do not have the same structure."))
	       (loop :for resource :in affected-resources
		     :for blueprint :in affected-blueprint
		     :for slots-spec = (slots-spec blueprint)
		     :collect (list resource
				    :resource resource
				    :blueprint blueprint
				    :update-slots (update-slots resource blueprint slots-spec)
				    :resource-slots (resource-slots resource blueprint slots-spec)))))
	   (resource-type-p (object)
	     (or (typep object 'resource)
		 (and (typep object 'list)
		      (and (every #'resource-type-p object)))))
	   (update-slots (resource blueprint slots-spec)
	     (loop :for spec :in slots-spec
		   :for slot-name = (getf spec :slot-name)
		   :for slot-value-resource = (slot-value resource slot-name)
		   :for slot-value-blueprint = (slot-value blueprint slot-name)
		   :when (and
			  (not (resource-type-p slot-value-blueprint))
			  (not (getf spec :resource))
			  (not (equal slot-value-resource slot-value-blueprint)))
		   :collect spec))
	   (resource-slots (resource blueprint slots-spec)
	     (declare (ignore resource))
	     (loop :for spec :in slots-spec
		   :when (or
			  (getf spec :resource)
			  (resource-type-p
			   (slot-value blueprint (getf spec :slot-name))))
		   :collect spec))
	   (set-the-recreate-slot (strategy)
	     (labels
		 ((update-slots (resource)
		    (getf (cdr (assoc resource strategy)) :update-slots))
		  (resource-slots (resource)
		    (getf (cdr (assoc resource strategy)) :resource-slots))
		  (recreate-p (resource)
		    (etypecase resource
		      (resource
		       (getf (cdr (assoc resource strategy)) :recreate))
		      (list
		       (some #'recreate-p resource))))
		  ((setf recreate-p) (new-value resource)
		    (setf (getf (cdr (assoc resource strategy)) :recreate)
			  new-value))
		  (immutable-slot-modified-p (resource)
		    (member-if
		     (lambda (spec) (getf spec :immutable))
		     (update-slots resource)))
		  (prerequisite-recreated-p (resource)
		    (member-if
		     (lambda (spec)
		       (recreate-p
			(slot-value resource (getf spec :slot-name))))
		     (resource-slots resource))))
	       (loop :for strategy-step :in (reverse strategy)
		     ;; The RECREATE property is available
		     ;; on prerequisites of every resource in
		     ;; STRATEGY-STEP because we iterate in
		     ;; reverse prerequisite order.
		     :for resource = (getf (cdr strategy-step) :resource)
		     :when (or (immutable-slot-modified-p resource)
			       (prerequisite-recreated-p resource))
		     :do (setf (recreate-p resource) t)))))
    (check-that-resource-and-blueprint-have-the-same-type)
    (let ((strategy
	    (make-strategy resource blueprint)))
      (set-the-recreate-slot strategy)
      (labels ((update-instance-instruction (resource)
		 (let ((update-slots
			 (getf (cdr (assoc resource strategy)) :update-slots))
		       (blueprint
			 (getf (cdr (assoc resource strategy)) :blueprint)))
		   (when update-slots
		     (list*
		      :update-instance
		      resource
		      (loop :for spec :in update-slots
			    :for slot-name = (getf spec :slot-name)
			    :collect slot-name
			    :collect (slot-value blueprint slot-name))))))
	       (recreate-instructions (resource)
		 (let ((update-instance-instructions
			 (update-instance-instruction resource)))
		   (if update-instance-instructions
		       (list update-instance-instructions
			     (list :create-resource resource))
		       (list (list :create-resource resource)))))
	       (update-instructions (resource)
		 (let ((update-instance-instructions
			 (update-instance-instruction resource)))
		   (if update-instance-instructions
		       (list update-instance-instructions
			     (list :update-resource resource))
		       nil)))
	       (delete-recreated-resources ()
		 (loop :for strategy-step :in strategy
		       :for resource = (first strategy-step)
		       :for recreate-p = (getf (rest strategy-step) :recreate)
		       :when recreate-p
		       :collect (list :delete-resource resource)))
	       (update-or-recreate-resources ()
		 (loop :for strategy-step :in (reverse strategy)
		       :for resource = (first strategy-step)
		       :for recreate-p = (getf (rest strategy-step) :recreate)
		       :append (if recreate-p
				   (recreate-instructions resource)
				   (update-instructions resource)))))
	(append (delete-recreated-resources)
		(update-or-recreate-resources))))))

;;;; End of file `resource.lisp'
