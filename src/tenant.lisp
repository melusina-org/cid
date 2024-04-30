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

(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class tenant (named-trait)
  nil
  (:base-table tenant)
  (:documentation "The class representing a TENANT.
When a CLSQL database is connected, tenants can be persisted on this database.
The contents of such a database can be examined with LIST-TENANTS
and FIND-TENANT."))

(defun make-tenant (&rest initargs &key name displayname)
  "Make a TENANT with the given attributes."
  (declare (ignore name displayname))
  (apply #'make-instance 'tenant initargs))

(defmethod address-components ((instance tenant))
  '())

(defun list-tenants ()
  "List existing tenants.
When a CLSQL database is connected, the list of tenants existing
in the database is returned."
  (clsql:select 'tenant :flatp t))

(defun find-tenant (designator)
  "Find the tenant associated to DESIGNATOR.
When DESIGNATOR is a string, it is used as a primary key to search
the table of persisted tenants in a connected CLSQL database."
  (flet ((find-tenant-by-name (name)
	   (caar (clsql:select 'tenant :where [= [slot-value 'tenant 'name] name]))))
    (etypecase designator
      (tenant
       designator)
      (string
       (find-tenant-by-name designator))
      (null
       nil))))


;;;;
;;;; Tenant Trait
;;;;

(defparameter *tenant* nil
  "The current TENANT used when creating projects, stewards and resources.")

(clsql:def-view-class tenant-trait nil
  ((tenant-name
    :type string
    :reader tenant-name)
   (tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-name
	      :foreign-key name
	      :set nil)
    :initarg :tenant
    :reader tenant))
  (:documentation "A trait for instances specific to a tenant.
The trait provides initialisation for the TENANT-NAME and TENANT slots
based on provided values."))

(defmethod initialize-instance :after ((instance tenant-trait) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((finalize-tenant-slot ()
	   (cond
	     ((and *tenant*
		   (not (slot-boundp instance 'tenant))
		   (not (slot-boundp instance 'tenant-name)))
	      (setf (slot-value instance 'tenant) *tenant*))
	     ((and (slot-boundp instance 'tenant)
		   (typep (slot-value instance 'tenant) 'string))
	      (with-slots (tenant) instance
		(setf tenant (or (find-tenant tenant)
				 (error "Cannot find tenant ~S." tenant)))))))
	 (finalize-tenant-name-slot ()
	   (when (and (slot-boundp instance 'tenant)
		      (not (slot-boundp instance 'tenant-name)))
	     (setf (slot-value instance 'tenant-name)
		   (name (slot-value instance 'tenant))))))
    (finalize-tenant-slot)
    (finalize-tenant-name-slot)))

;;;; End of file `tenant.lisp'
