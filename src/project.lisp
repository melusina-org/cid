;;;; project.lisp — Projects for El Cid

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

(clsql:def-view-class project ()
  ((projectid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :projectid)
   (tenantid
    :type integer
    :initarg :tenantid)
   (pathname
    :accessor project-pathname
    :db-constraints :unique
    :type string
    :initarg :pathname)
   (displayname
    :accessor project-displayname
    :type string
    :initarg :displayname)
   (tenant
    :accessor project-tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenantid
	      :foreign-key tenantid
	      :set nil)))
  (:base-table project))

(defun find-project (designator)
  "Find the project associated to DESIGNATOR."
  (typecase designator
    (project
     designator)
    (string
     (caar (clsql:select 'project :where [= [slot-value 'project 'pathname] designator])))
    (integer
     (caar (clsql:select 'project :where [= [slot-value 'project 'projectid] designator])))))

(defun make-project (&key pathname displayname tenant)
  "Make a PROJECT with the given attributes."
  (let* ((tenant
	   (find-tenant tenant))
	 (project
	   (make-instance 'project
			  :tenantid (slot-value tenant 'tenantid)
			  :pathname pathname
			  :displayname displayname)))
    (clsql:update-records-from-instance project)
    (values project)))

;;;; End of file `project.lisp'
