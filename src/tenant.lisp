;;;; tenant.lisp — Tenant for El Cid

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

(defvar *tenant* nil
  "The current `TENANT' used when creating projects, stewards and resources.")

(defvar *tenant-directory* (make-hash-table :test #'equal)
  "The directory of known tenants.")

(defclass tenant (named-trait)
  nil
  (:documentation "The class representing a TENANT."))

(defun make-tenant (&rest initargs &key name displayname)
  "Make a TENANT with the given attributes.
Unless the `*TENANT-DIRECTORY*' is NIL, the tenant is also
added to the directory of known tenants and can be looked
up using `FIND-TENANT'."
  (declare (ignore name displayname))
  (flet ((return-existing-tenant-when-available (tenant)
	   (let ((existing-tenant
		   (and *tenant-directory*
			(gethash (name tenant) *tenant-directory*))))
	     (when (and existing-tenant
			(string=
			 (name tenant)
			 (name existing-tenant))
			(string=
			 (displayname tenant)
			 (displayname existing-tenant)))
	       (return-from make-tenant existing-tenant))))
	 (fail-when-name-is-taken (tenant)
	   (when (and *tenant-directory*
		      (gethash (name tenant) *tenant-directory*))
	     (error "A tenant named ~A already exists." (name tenant))))
	 (maybe-add-to-directory (tenant)
	   (when *tenant-directory*
	     (setf (gethash (name tenant) *tenant-directory*) tenant))))
    (let ((tenant
	    (apply #'make-instance 'tenant initargs)))
      (return-existing-tenant-when-available tenant)
      (fail-when-name-is-taken tenant)
      (maybe-add-to-directory tenant)
      (values tenant))))

(defmethod print-object ((instance tenant) stream)
  (flet ((print-readably ()
	   (write-persistent-object instance stream 'tenant
				    '((:name name)
				      (:displayname displayname))))
	 (print-unreadably ()
	   (with-slots (name displayname) instance
	     (print-unreadable-object (instance stream :type t :identity t)
	       (format stream "~A ~A" name displayname)))))
    (if *print-readably*
	(print-readably)
	(print-unreadably))))

(defmethod persistent-constructor ((class (eql 'tenant)))
  #'make-tenant)

(defun list-tenants ()
  "List existing tenants."
  (alexandria:hash-table-values *tenant-directory*))

(defun find-tenant (designator)
  "Find the tenant associated to DESIGNATOR.
When DESIGNATOR is a string, it is used as a primary key to search
the table of known tenants."
  (flet ((find-tenant-by-name (name)
	   (gethash name *tenant-directory*)))
    (etypecase designator
      (tenant
       designator)
      (string
       (find-tenant-by-name designator))
      (null
       nil))))

;;;; End of file `tenant.lisp'
