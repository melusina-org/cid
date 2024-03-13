;;;; property-list.lisp — Property-List Steward for El Cid

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
;;;; Steward
;;;;

(clsql:def-view-class property-list (steward)
  ((steward-class
    :allocation :class
    :initform 'property-list
    :type symbol)
   (description
    :allocation :class
    :initform "A steward that owns named text properties."
    :type string)
   (properties
    :initform nil
    :db-kind :virtual))
  (:documentation
   "A steward that owns named text properties.
These text properties are not persistent."))

(defun make-property-list (&rest initargs &key tenant project pathname)
  "Make a property-list steward."
  (declare (ignore initargs))
  (funcall #'make-instance 'property-list
	   :tenant tenant
	   :project project
	   :pathname pathname))

;;;;
;;;; Resource
;;;;

(clsql:def-view-class property (resource)
  ((steward-class
    :type symbol
    :initform 'property-list
    :allocation :class)
   (value
    :type string
    :initarg :value
    :accessor property-value))
  (:documentation
   "A named text property. The name of the property is its pathname."))

(defun make-property (&rest initargs &key property-list pathname value)
  "Make a property steward."
  (declare (ignore initargs))
  (check-type property-list property-list)
  (funcall #'make-instance 'property
	   :steward property-list
	   :pathname pathname
	   :value value))

(defmethod examine-resource append ((instance property))
  (with-slots (name value) instance
    (list
     :value value)))

(defmethod list-resource-identifiers ((instance property-list) (resource-class (eql 'property)))
  (loop :for (identifier . value) :in (slot-value instance 'properties)
	:collect identifier))

(defmethod create-resource ((instance property))
  (with-slots (steward identifier state pathname value) instance
    (push (cons pathname value)
	  (slot-value steward 'properties))
    (setf identifier pathname)
    (setf state t)))

(defmethod delete-resource ((instance property))
  (with-slots (steward identifier state pathname value) instance
    (remove pathname (slot-value steward 'properties) :test #'string= :key #'car)
    (setf identifier nil)
    (setf state nil)))

(defmethod update-resource-from-instance ((instance property))
  (with-slots (steward identifier pathname value) instance
    (remove identifier (slot-value steward 'properties) :test #'string= :key #'car)
    (setf identifier pathname)
    (push (cons identifier value)
	  (slot-value steward 'properties))))

(defmethod import-resource ((instance property-list) &key pathname description identifier)
  (flet ((setf-description-and-identifier (resource)
	   (setf (slot-value resource 'description) description
		 (slot-value resource 'identifier) identifier))
	 (find-value-or-fail ()
	   (or (alexandria:assoc-value (slot-value instance 'properties) identifier :test #'string=)
	       (resource-error
		'import-resource nil 
		"Cannot import resource"
		"There is no resource with identifier ~A in property list ~A." identifier (steward-pathname instance)))))
			       
  (let ((property
	  (make-property :property-list instance
			 :pathname pathname
			 :value (find-value-or-fail))))
    (setf-description-and-identifier property)
    (values property))))

;;;; End of file `property-list.lisp'
