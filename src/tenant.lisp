;;;; tenant.lisp — Tenant for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class tenant ()
  ((tenantid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :tenantid)
   (pathname
    :accessor tenant-pathname
    :type string
    :db-constraints :unique
    :initarg :pathname
    :description "The PATHNAME element designates the tenant.
This is a string of characters from the portable filename character set.")
   (displayname
    :accessor tenant-displayname
    :type string
    :initarg :displayname
    :description "The DISPLAYNAME element is used in informational screens to denote the tenant."))
  (:base-table tenant))

(defun find-tenant (designator)
  "Find the tenant associated to DESIGNATOR."
  (typecase designator
    (tenant
     designator)
    (string
     (caar (clsql:select 'tenant :where [= [slot-value 'tenant 'pathname] designator])))
    (integer
     (caar (clsql:select 'tenant :where [= [slot-value 'tenant 'tenantid] designator])))
    (nil
     nil)))

(defun make-tenant (&key pathname displayname)
  "Make a TENANT with the given attributes."
  (let ((tenant
	  (make-instance 'tenant :pathname pathname :displayname displayname)))
    (clsql:update-records-from-instance tenant)
    (values tenant)))


;;;;
;;;; Tenant Scope
;;;;

(clsql:def-view-class tenant-scope ()
  ((tenantid
    :type integer
    :initarg :tenantid)
   (tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenantid
	      :foreign-key tenantid
	      :set nil)))
  (:documentation "The TENANT-SCOPE contains traits for views specific to a tenant."))

(defmacro ensure-tenant-scope ((designator) &body body)
  "Run BODY and ensures the result is a value of type TENANT-SCOPE that belongs to DESIGNATOR.
There is an implicit `WITH-SLOTS (TENANTID)' around BODY."
  (let ((tenant (gensym))
	(some-tenant-scope (gensym)))
    `(let ((,tenant
	     (find-tenant ,designator)))
       (when ,tenant
	 (with-slots (tenantid) ,tenant
	   (let ((,some-tenant-scope (progn ,@body)))
	     (and ,some-tenant-scope
		  (eq tenantid (slot-value ,some-tenant-scope 'tenantid))
		  ,some-tenant-scope)))))))

;;;; End of file `tenant.lisp'
