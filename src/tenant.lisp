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
  (:base-table tenant))

(defun list-tenants ()
  "List existing tenants."
  (clsql:select 'tenant :flatp t))

(defun find-tenant (designator)
  "Find the tenant associated to DESIGNATOR."
  (flet ((find-by-name (name)
	   (caar (clsql:select 'tenant :where [= [slot-value 'tenant 'name] name]))))
    (etypecase designator
      (tenant
       designator)
      (string
       (find-by-name designator))
      (null
       nil))))

(defun make-tenant (&rest initargs &key name displayname)
  "Make a TENANT with the given attributes."
  (declare (ignore name displayname))
  (apply #'make-instance 'tenant initargs))

;;;; End of file `tenant.lisp'
