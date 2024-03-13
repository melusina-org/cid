;;;; project.lisp — Projects for El Cid

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

(clsql:def-view-class project (named-trait)
  ((tenant-name
    :type string
    :db-kind :key
    :initarg :tenant-name
    :reader tenant-name)
   (tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-name
	      :foreign-key name
	      :set nil)
    :initarg :tenant
    :reader tenant))
  (:base-table project))

(defmethod initialize-instance :after ((instance project) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((finalize-tenant-slot ()
	   (when (slot-boundp instance 'tenant)
	     (with-slots (tenant) instance
	       (unless (typep tenant 'tenant)
		 (setf tenant (find-tenant tenant))))))
	 (finalize-tenant-name-slot ()
	   (when (and (slot-boundp instance 'tenant)
		      (not (slot-boundp instance 'tenant-name)))
	     (with-slots (tenant) instance
	       (setf (slot-value instance 'tenant-name)
		     (tenant-name tenant))))))
    (finalize-tenant-slot)
    (finalize-tenant-name-slot)))

(defun list-projects (&key tenant)
  "List existing tenants."
  (flet ((return-early-if-tenant-does-not-exist ()
	   (setf tenant (find-tenant tenant))
	   (unless tenant
	     (return-from list-projects nil)))
	 (list-for-tenant (tenant-name)
	   (clsql:select
	    'project
	    :where [= [slot-value 'project 'tenant-name] tenant-name]
	    :flatp t))
	 (list-every-project ()
	   (clsql:select 'project :flatp t)))
    (cond
      (tenant
       (return-early-if-tenant-does-not-exist)
       (list-for-tenant (name tenant)))
      (t
       (list-every-project)))))

(defun find-project (designator &key tenant)
  "Find the project associated to DESIGNATOR."
  (flet ((return-early-if-tenant-does-not-exist ()
	   (setf tenant (find-tenant tenant))
	   (unless tenant
	     (return-from find-project nil)))
	 (find-by-name (tenant-name name)
	   (caar
	    (clsql:select
	     'project
	     :where [and [= [slot-value 'project 'name] name]
                         [= [slot-value 'project 'tenant-name] tenant-name]]))))
    (etypecase designator
      (project
       designator)
      (string
       (return-early-if-tenant-does-not-exist)
       (find-by-name (name tenant) designator))
      (null
       nil))))

(defun make-project (&rest initargs &key name displayname tenant)
  "Make a PROJECT with the given attributes."
  (declare (ignore initargs))
  (let ((tenant
	  (or (find-tenant tenant)
	      (error "Cannot find tenant ~S." tenant))))
    (make-instance 'project
		   :tenant-name (name tenant)
		   :tenant tenant
		   :name name
		   :displayname displayname)))

;;;; End of file `project.lisp'
