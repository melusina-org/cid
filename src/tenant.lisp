;;;; tenant.lisp — Tenant for El Cid

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

;;;; End of file `tenant.lisp'
