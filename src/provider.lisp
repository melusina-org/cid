;;;; provider.lisp — Operation provider for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use,
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

(in-package #:org.melusina.cid)

(defparameter *providers* (make-hash-table)
  "The providers currently used by the system.
The key in this table must be a keyword.")

(defclass provider nil
  ((pathname
    :initarg :pathname
    :initform (error "A PROVIDER requires a :PATHNAME slot.")
    :documentation "A name for the PROVIDER.
The fully qualified name of the PROVIDER, which should be a safe Unix path.")
   (description
    :initarg :description
    :initform nil
    :documentation "A short description of the PROVIDER."))
  (:documentation "The class represents providers responsible for creating
resources consumed by the deployment of software components.
Some examples of providers are the localhost, a configured docker engine,
a remote host accesible over SSH, a kubernetes cluster hosted in a public
cloud, among many other possibilities."))

(defmethod print-object ((instance provider) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S" (if (slot-boundp instance 'pathname)
			 (slot-value instance 'pathname)
			 "(no pathname)"))))

(defmethod describe-object ((instance provider) stream)
  (format stream "~&~A is a provider of type ~A."
	  instance (type-of instance))
  (with-slots (description) instance
    (when description
      (format stream "~&Description: ~A" description)))
  (values))

(defmethod initialize-instance :after ((instance provider)
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((designator
	  (make-keyword (string-upcase (slot-value instance 'pathname)))))
    (setf (gethash designator *providers*) instance))
  (values))

(defun find-provider (designator)
  "The provider designated by DESIGNATOR.
When DESIGNATOR is a PROVIDER, it is immediately returned. When DESIGNATOR
is a keyword, it corresponding entry in *PROVIDERS* is used."
  (cond
    ((typep designator 'provider)
     designator)
    ((keywordp designator)
     (gethash designator *providers*))))

;;;; End of file `provider.lisp'
