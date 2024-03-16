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

(clsql:def-view-class project (named-trait tenant-trait)
  nil
  (:base-table project))

(defmethod address-components ((instance project))
  '(tenant-name))

(defun list-projects (&key (tenant *tenant*))
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
  (declare (ignore name displayname tenant))
  (apply #'make-instance 'project initargs))

(defparameter *project* nil
  "The current PROJECT used in operations.")

(clsql:def-view-class project-trait nil
  ((project-name
    :type string
    :db-kind :key
    :reader project-name)
   (project
    :db-kind :join
    :db-info (:join-class project
	      :home-key project-name
	      :foreign-key name
	      :set nil)
    :initarg :project
    :reader project))
  (:documentation "A trait for instances specific to a project.
The trait provides initialisation for the PROJECT-NAME and PROJECT slots
based on provided values."))

(defmethod initialize-instance :after ((instance project-trait) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((finalize-project-slot ()
	   (cond
	     ((and (slot-boundp instance 'project)
		   (slot-boundp instance 'tenant)
		   (not (typep (slot-value instance 'project) 'project)))
	      (with-slots (tenant project) instance
		(setf project (or (find-project project :tenant tenant)
				  (error "Cannot find project ~S for tenant ~A." project tenant)))))
	     ((and (not (slot-boundp instance 'project))
		   (not (slot-boundp instance 'project-name))
		   *project*)
	      (setf (slot-value instance 'project) *project*))))
	 (finalize-project-name-slot ()
	   (when (and (slot-boundp instance 'project)
		      (typep (slot-value instance 'project) 'project)
		      (not (slot-boundp instance 'project-name)))
	     (setf (slot-value instance 'project-name) (name (slot-value instance 'project)))))
	 (initialize-tenant-slot-from-project-slot ()
	   (when (and (not (slot-boundp instance 'tenant))
		      (slot-boundp instance 'project)
		      (typep (slot-value instance 'project) 'project))
	     (with-slots (project) instance
	       (setf (slot-value instance 'tenant) (tenant project))))))
    ;(break "initialize-instance :after ((instance project))")
    (initialize-tenant-slot-from-project-slot)
    (finalize-project-slot)
    (finalize-project-name-slot)))

;;;; End of file `project.lisp'
