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

(clsql:def-view-class project (tenant-scope)
  ((projectid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :projectid)
   (pathname
    :accessor project-pathname
    :db-constraints :unique
    :type string
    :initarg :pathname)
   (displayname
    :accessor project-displayname
    :type string
    :initarg :displayname))
  (:base-table project))

(defun project-tenant (project)
  "The tenant of PROJECT."
  (slot-value project 'tenant))

(defmethod print-object ((instance project) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (pathname displayname tenant) instance
      (format stream "~S :PATHNAME ~S :TENANT ~S"
	      displayname pathname (tenant-displayname tenant)))))

(defun list-projects (&key tenant)
  "List existing tenants."
  (cond
    (tenant
     (setf tenant (find-tenant tenant))
     (unless tenant
       (return-from list-projects nil))
     (clsql:select 'project
		   :where [= [slot-value 'project 'tenantid]
		             (slot-value tenant 'tenantid)]
		   :flatp t))
    (t
     (clsql:select 'project :flatp t))))

(defun find-project (designator tenant)
  "Find the project associated to DESIGNATOR."
  (ensure-tenant-scope (tenant)
    (typecase designator
      (project
       designator)
      (string
       (caar (clsql:select 'project :where [and [= [slot-value 'project 'pathname] designator]
			                        [= [slot-value 'project 'tenantid] tenantid]])))
      (integer
       (caar (clsql:select 'project :where [= [slot-value 'project 'projectid] designator]))))))

(defun make-project (&rest initargs &key pathname displayname tenant)
  "Make a PROJECT with the given attributes."
  (declare (ignore initargs))
  (let ((tenant
	  (or (find-tenant tenant)
	      (error "Cannot find tenant ~S." tenant))))
    (labels
	((other-attributes-match-p (project)
	   (unless project
	     (return-from other-attributes-match-p project))
	   (unless (string-equal displayname (slot-value project 'displayname))
	     (error "Project attribute mismatch.
A project with the qualified pathname ~A exists
but some attribute differs. The attribute DISPLAYNAME in the existing project
is ~S instead of ~S."
		    (slot-value project 'pathname)
		    (slot-value project 'displayname) displayname))
	   (unless (equal (slot-value tenant 'tenantid) (slot-value project 'tenantid))
	     (error "Project attribute mismatch.
A project with the qualified pathname ~A exists
but some attribute differs. The attribute TENANTID in the existing project
is ~S instead of ~S."
		    (slot-value project 'pathname)
		    (slot-value project 'tenantid) (slot-value tenant 'tenantid)))
	   project))
      (let ((project
	      (or (other-attributes-match-p
		   (find-project pathname tenant))
		  (make-instance 'project
				 :tenantid (slot-value tenant 'tenantid)
				 :pathname pathname
				 :displayname displayname))))
	(clsql:update-records-from-instance project)
	(values project)))))

;;;; End of file `project.lisp'
