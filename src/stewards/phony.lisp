;;;; phony.lisp — Phony Steward for El Cid

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

(clsql:def-view-class phony-steward (steward)
  ((description
    :db-kind :virtual
    :allocation :class
    :initform "A steward that does not actually administrate resources."
    :type string)
   (resource-identifiers
    :db-kind :virtual
    :initarg :resource-identifiers
    :initform nil
    :documentation "The list of resource identifiers that have been created."))
  (:documentation
   "A steward for that do not actually own resources.
For phony resources, every step of the lifecycle is a no-operation."))

(defun make-phony-steward (&rest initargs &key tenant project name displayname pathname resource-identifiers)
  "Make a PHONY-STEWARD with the given parameters."
  (declare (ignore tenant project name displayname pathname resource-identifiers))
  (apply #'make-instance 'phony-steward initargs))

(clsql:def-view-class phony-resource (resource)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'phony-steward
    :allocation :class)
   (create-error-p
    :type boolean
    :initarg :create-error-p
    :initform nil
    :documentation "When set, this flag triggers an error when the resource is created.")
   (delete-error-p
    :type boolean
    :initarg :delete-error-p
    :initform nil
    :documentation "When set, this flag triggers an error when the resource is deleted."))
  (:documentation
   "A phony resource, every step of the lifecycle is a no-operation."))

(defun make-phony-resource (&rest initargs &key phony-steward name displayname description)
  "Make a phony resource."
  (declare (ignore displayname description))
  (apply #'make-instance 'phony-resource
	 :steward phony-steward
	 (remove-property initargs :phony-steward)))

(defmethod create-resource ((instance phony-resource))
  (with-slots (steward name identifier state create-error-p) instance
    (with-slots (resource-identifiers) steward
      (when create-error-p
	(resource-error
	 'create-resource instance
	 "Cannot create phony resource."
	 "It is not possible for steward ~A to create the phony resource ~A.
This resource is configured, so that an error is triggered when an attempt
is made to create it."
	 steward instance))
      (when (member name resource-identifiers :test #'string=)
	(resource-error
	 'create-resource instance
	 "Cannot create phony resource, resource already exists."
	 "It is not possible for steward ~A to create the phony resource ~A.
This resource already exists."
	 steward instance))
      (setf identifier name
	    state t)
      (push name resource-identifiers))))

(defmethod delete-resource ((instance phony-resource))
  (with-slots (steward identifier state delete-error-p) instance
    (with-slots (resource-identifiers) steward
      (unless (member identifier resource-identifiers :test #'string=)
	(resource-error
	 'create-resource instance
	 "Cannot delete phony resource, resource does not exist."
	 "It is not possible for steward ~A to delete the phony resource ~A.
This resource does not actually exist."
	 steward instance))
      (when delete-error-p
	(resource-error
	 'delete-resource instance
	 "Cannot delete phony resource."
	 "It is not possible for steward ~A to delete the phony resource ~A.
This resource is configured, so that an error is triggered when an attempt
is made to delete it."
	 steward instance))
      (setf resource-identifiers
	    (delete identifier resource-identifiers :test #'string=)
	    identifier nil
	    state nil))))

(defmethod list-resource-identifiers ((steward phony-steward) (resource-class (eql 'phony-resource)))
  (slot-value steward 'resource-identifiers))

(defmethod update-instance-from-resource ((instance phony-resource))
  (with-slots (state identifier) instance
    (if (member identifier (list-resource-identifiers (steward instance) 'phony-resource)
		:test #'string=)
	(setf state t)
	(setf state nil
	      identifier nil))))

;;;; End of file `phony.lisp'
