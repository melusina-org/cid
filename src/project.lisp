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

(defvar *project* nil
  "The current `PROJECT' used when creating stewards and resources.")

(defvar *project-directory* (make-hash-table :test #'equal)
  "The directory of known projects.")

(defun project-directory-key (project)
  "The key used in `*PROJECT-DIRECTORY*' to store tenant."
  (list (name (tenant project)) (name project)))

(defclass project (named-trait)
  ((tenant
    :initarg :tenant
    :initform *tenant*
    :reader tenant
    :type tenant))
  (:documentation "The class representing a PROJECT."))

(defmethod initialize-instance :after ((instance project) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((support-initialize-tenant-slot-with-designator ()
	   (cond
	     ((typep (slot-value instance 'tenant) 'string)
	      (with-slots (tenant) instance
		(setf tenant (or (find-tenant tenant)
				 (error "Cannot find tenant ~S." tenant))))))))
    (support-initialize-tenant-slot-with-designator)))

(defun make-project (&rest initargs &key name displayname tenant)
  "Make a PROJECT with the given attributes.
Unless the `*PROJECT-DIRECTORY*' is NIL, the project is also
added to the directory of known projects and can be looked
up using `FIND-PROJECT'."
  (declare (ignore name displayname tenant))
  (flet ((return-existing-project-when-available (project)
	   (let ((existing-project
		   (and *project-directory*
			(gethash (project-directory-key project) *project-directory*))))
	     (when (and existing-project
			(eq
			 (tenant project)
			 (tenant existing-project))
			(string=
			 (name project)
			 (name existing-project))
			(string=
			 (displayname project)
			 (displayname existing-project)))
	       (return-from make-project existing-project))))
	 (fail-when-name-is-taken (project)
	   (when (and *project-directory*
		      (gethash (project-directory-key project) *project-directory*))
	     (error "A project named ~A already exists for tenant ~A."
		    (name project) (name (tenant project)))))
	 (maybe-add-to-directory (project)
	   (when *project-directory*
	     (setf (gethash (project-directory-key project) *project-directory*) project))))
    (let ((project
	    (apply #'make-instance 'project initargs)))
      (return-existing-project-when-available project)
      (fail-when-name-is-taken project)
      (maybe-add-to-directory project)
      (values project))))

(defmethod print-object ((instance project) stream)
  (flet ((print-readably ()
	   (write-persistent-object instance stream))
	 (print-unreadably ()
	   (with-slots (tenant name displayname) instance
	     (print-unreadable-object (instance stream :type t :identity t)
	       (format stream "~A:~A ~A" (name tenant) name displayname)))))
    (if *print-readably*
	(print-readably)
	(print-unreadably))))

(defmethod persistent-constructor ((class (eql 'project)))
  #'make-project)

(defmethod persistent-slots append ((instance project))
  '((:initarg :tenant
     :slot-name tenant)
    (:initarg :name
     :slot-name name)
    (:initarg :displayname
     :slot-name displayname)))

(defun list-projects (&key (tenant *tenant*))
  "List existing projects."
  (flet ((return-early-if-tenant-does-not-exist ()
	   (setf tenant (find-tenant tenant))
	   (unless tenant
	     (return-from list-projects nil)))
	 (list-for-tenant (tenant-name)
	   (loop :for key :being :the :hash-key
		 :of *project-directory*
		 :using (hash-value project)
		 :when (equal (first key) tenant-name)
		 :collect project))
	 (list-every-project ()
	   (alexandria:hash-table-values *project-directory*)))
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
	   (gethash (list tenant-name name) *project-directory*)))
    (etypecase designator
      (project
       designator)
      (string
       (return-early-if-tenant-does-not-exist)
       (find-by-name (name tenant) designator))
      (null
       nil))))

;;;; End of file `project.lisp'
