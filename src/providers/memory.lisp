;;;; memory.lisp — Memory Provider for El Cid

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

(defclass memory (provider)
  ((pathname
    :initarg :pathname
    :initform "memory"
    :documentation "A name for the PROVIDER.
The fully qualified name of the PROVIDER, which should be a safe Unix path.")
   (description
    :initform "A provider for in-memory resources.
In-memory resources represent information stored in the operator's computer
memory."
    :documentation "A short description of the PROVIDER.")
   (resources
    :initform (make-hash-table)
    :documentation "The in-memory resources created by the PROVIDER."))
  (:documentation
   "A provider for null resources.
For null resources, every step of the lifecycle is a no-operation."))

(defun make-memory ()
  "Make a memory provider."
  (or (find-provider :memory)
      (make-instance 'memory)))

(defclass memory-text (resource)
  ((text
    :initarg :text
    :initform (error "A MEMORY-TEXT requires a TEXT.")
    :documentation "A text stored in memory.")
   (provider-class
    :allocation :class
    :initform 'memory))
  (:documentation
   "This class represents a text stored in memory."))

(defun make-memory-text (&rest initargs
			 &key pathname description provider identification text)
  "Make a MEMORY-TEXT resource."
  (declare (ignore pathname description identification text))
  (unless (typep provider 'memory)
    (error "A MEMORY-TEXT must be associated to a MEMORY provider."))
  (apply #'make-instance 'memory-text initargs))

(defmethod actually-create-resource ((instance memory-text))
  (with-slots (pathname identification provider text) instance
    (setf identification (cid:random-string 7))
    (setf (gethash identification (slot-value provider 'resources))
	  text)))

(defmethod actually-read-resource ((instance memory-text))
  (with-slots (identification provider text) instance
    (multiple-value-bind (actual-text bound-p)
	(gethash identification (slot-value provider 'resources) nil)
      (if bound-p
	  (setf text actual-text)
	  (setf identification nil)))))

(defmethod actually-update-resource ((instance memory-text))
  (with-slots (identification provider text) instance
    (setf (gethash identification (slot-value provider 'resources)) text)))

(defmethod actually-delete-resource ((instance memory-text))
  (with-slots (identification provider) instance
    (unless (remhash identification (slot-value provider 'resources))
      (warn "Resource did not exist."))
    (setf identification nil)))
  
(defmethod actually-examine-resource append ((instance memory-text))
  (list :text (slot-value instance 'text)))

;;;; End of file `memory.lisp'
