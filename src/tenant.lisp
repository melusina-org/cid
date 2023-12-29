;;;; tenant.lisp — Tenant for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class tenant ()
  ((pathname
    :accessor tenant-pathname
    :type string
    :db-kind :key
    :initarg :pathname
    :description "The PATHNAME element designates the tenant.
This is a string of characters from the portable filename character set.")
   (displayname
    :accessor tenant-displayname
    :type string
    :initarg :displayname
    :description "The DISPLAYNAME element is used in informational screens to denote the tenant."))
  (:base-table tenant))

(defmethod print-object ((instance tenant) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'pathname)
	       (slot-boundp instance 'displayname))
      (with-slots (pathname displayname) instance
	(format stream "~A ~S" pathname displayname)))))

(defun list-tenants ()
  "List existing tenants."
  (clsql:select 'tenant :flatp t))

(defun find-tenant (designator)
  "Find the tenant associated to DESIGNATOR."
  (typecase designator
    (tenant
     designator)
    (string
     (caar (clsql:select 'tenant :where [= [slot-value 'tenant 'pathname] designator])))
    (null
     nil)))

(defun make-tenant (&rest initargs &key pathname displayname)
  "Make a TENANT with the given attributes."
  (declare (ignore pathname displayname))
  (apply #'make-instance 'tenant initargs))

;;;; End of file `tenant.lisp'
