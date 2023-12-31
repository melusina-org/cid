;;;; steward.lisp — Resource Steward for El Cid

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

(clsql:def-view-class steward ()
  ((tenant-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader tenant-pathname)
   (project-pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader project-pathname)
   (pathname
    :type string
    :db-kind :key
    :db-constraints :not-null
    :initarg :pathname
    :reader steward-pathname
    :documentation "A name for the STEWARD.
The fully qualified name of the STEWARD, which should be a safe Unix path.")
   (steward-class
    :type symbol
    :allocation :class
    :documentation "The class a steward for this resource must belong to.
This slot is only bound for concrete classes.")
   (description
    :type string
    :initarg :description
    :initform nil
    :documentation "A short description of the STEWARD.")
   (tenant
    :reader steward-tenant
    :initarg :tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-pathname
	      :foreign-key pathname
	      :set nil))
   (project
    :reader steward-project
    :initarg :project
    :db-kind :join
    :db-info (:join-class project
	      :home-key (tenant-pathname project-pathname)
	      :foreign-key (tenant-pathname pathname)
	      :set nil)))
  (:documentation "The class represents stewards responsible for
resources consumed by the deployment of software components.

Some examples of STEWARDS are the localhost, a configured docker engine,
a remote host accesible over SSH, a kubernetes cluster hosted in
a public cloud, among many other possibilities."))

(defmethod print-object ((instance steward) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'tenant-pathname)
	       (slot-boundp instance 'project-pathname)
	       (slot-boundp instance 'pathname))
      (with-slots (tenant-pathname project-pathname pathname) instance
	(format stream "~A ~A ~A"
		tenant-pathname project-pathname pathname)))))

(defmethod initialize-instance :after ((instance steward) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((initialize-tenant-slot-from-project-slot ()
	   (when (and (not (slot-boundp instance 'tenant))
		      (slot-boundp instance 'project)
		      (typep (slot-value instance 'project) 'project))
	     (with-slots (project) instance
	       (setf (slot-value instance 'tenant)
		     (project-tenant project)))))
	 (finalize-tenant-slot ()
	   (when (slot-boundp instance 'tenant)
	     (with-slots (tenant) instance
	       (unless (typep tenant 'tenant)
		 (setf tenant (find-tenant tenant))))))
	 (finalize-tenant-pathname-slot ()
	   (when (and (slot-boundp instance 'tenant)
		      (not (slot-boundp instance 'tenant-pathname)))
	     (with-slots (tenant tenant-pathname) instance
	       (setf (slot-value instance 'tenant-pathname)
		     (tenant-pathname tenant)))))
	 (finalize-project-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-boundp instance 'tenant))
	     (with-slots (project tenant) instance
	       (unless (typep project 'project)
		 (setf project (find-project project :tenant tenant))))))
	 (finalize-project-pathname-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-value instance 'project)
		      (not (slot-boundp instance 'project-pathname)))
	     (with-slots (project) instance
	       (setf (slot-value instance 'project-pathname)
		     (project-pathname project))))))
    (initialize-tenant-slot-from-project-slot)
    (finalize-tenant-slot)
    (finalize-tenant-pathname-slot)
    (finalize-project-slot)
    (finalize-project-pathname-slot)))

(defun find-steward (designator &key tenant project steward-class)
  "The steward designated by DESIGNATOR.
When DESIGNATOR is a STEWARD, it is immediately returned. When
DESIGNATOR is a string, it is interpreted as a pathname to search
stewards in the given project."
  (when (typep designator 'steward)
    (return-from find-steward designator))
  (when tenant
    (setf tenant (find-tenant tenant)))
  (when project
    (setf project (find-project project :tenant tenant))
    (unless tenant
      (setf tenant (project-tenant project))))
  (unless project
    (error "A PROJECT is required to find a steward."))
  (unless steward-class
    (error "A STEWARD-CLASS is required to find a steward."))
  (flet ((find-by-pathname (pathname)
	   (caar
	    (clsql:select
	     steward-class
 	     :where [and [= [tenant-pathname] (tenant-pathname project)]
                         [= [project-pathname] (project-pathname project)]
                         [= [pathname] pathname]]))))
    (etypecase designator
      (string
       (find-by-pathname designator)))))


;;;;
;;;; Configure
;;;;

(defgeneric configure-steward (steward)
  (:documentation
   "Configure the STEWARD.
The configuration lifecycle step of a STEWARD prepares a steward
to operate.

Depending on the specific steward, this could verify the connectivity
to services, the ability to write on local file systems, etc."))

(defmethod configure-steward ((instance steward))
  (declare (ignore instance))
  (values))

(defmethod configure-steward :around ((instance steward))
  "Ensure that CONFIGURE-STEWARD returns the INSTANCE."
  (call-next-method)
  (values instance))

;;;; End of file `steward.lisp'
