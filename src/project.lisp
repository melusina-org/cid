;;;; project.lisp — Projects for El Cid

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

(clsql:def-view-class project ()
  ((tenant-pathname
    :type string
    :db-kind :key
    :initarg :tenant-pathname
    :reader tenant-pathname)
   (pathname
    :db-kind :key
    :type string
    :initarg :pathname
    :reader project-pathname)
   (displayname
    :accessor project-displayname
    :type string
    :initarg :displayname)
   (tenant
    :reader project-tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-pathname
	      :foreign-key pathname
	      :set nil)))
  (:base-table project))

(defmethod print-object ((instance project) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'pathname)
	       (slot-boundp instance 'tenant-pathname))
      (with-slots (tenant-pathname pathname displayname) instance
	(format stream "~A ~A ~S"
		tenant-pathname pathname displayname)))))

(defun list-projects (&key tenant)
  "List existing tenants."
  (cond
    (tenant
     (setf tenant (find-tenant tenant))
     (unless tenant
       (return-from list-projects nil))
     (clsql:select 'project
		   :where [= [slot-value 'project 'tenant-pathname]
		             (slot-value tenant 'pathname)]
		   :flatp t))
    (t
     (clsql:select 'project :flatp t))))

(defun find-project (designator &key tenant)
  "Find the project associated to DESIGNATOR."
  (flet ((return-early-if-tenant-does-not-exist ()
	   (when tenant
	     (setf tenant (find-tenant tenant)))
	   (unless tenant
	     (return-from find-project nil)))
	 (find-by-pathname (pathname tenant-pathname)
	   (caar
	    (clsql:select
	     'project
	     :where [and [= [slot-value 'project 'pathname] pathname]
                         [= [slot-value 'project 'tenant-pathname] tenant-pathname]]))))
    (etypecase designator
      (project
       designator)
      (string
       (return-early-if-tenant-does-not-exist)
       (find-by-pathname designator (tenant-pathname tenant)))
      (null
       nil))))

(defun make-project (&rest initargs &key pathname displayname tenant)
  "Make a PROJECT with the given attributes."
  (declare (ignore initargs))
  (let ((tenant
	  (or (find-tenant tenant)
	      (error "Cannot find tenant ~S." tenant))))
    (make-instance 'project
		   :tenant-pathname (tenant-pathname tenant)
		   :pathname pathname
		   :displayname displayname)))

;;;; End of file `project.lisp'
