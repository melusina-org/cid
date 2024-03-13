;;;; steward.lisp — Resource Steward for El Cid

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

(clsql:def-view-class steward (named-trait)
  ((tenant-name
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader tenant-name)
   (project-name
    :type string
    :db-kind :key
    :db-constraints :not-null
    :reader project-name)
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
    :reader tenant
    :initarg :tenant
    :db-kind :join
    :db-info (:join-class tenant
	      :home-key tenant-name
	      :foreign-key name
	      :set nil))
   (project
    :reader project
    :initarg :project
    :db-kind :join
    :db-info (:join-class project
	      :home-key (tenant-name project-name)
	      :foreign-key (tenant-name name)
	      :set nil)))
  (:documentation "The class represents stewards responsible for
resources consumed by the deployment of software components.

Some examples of STEWARDS are the localhost, a configured docker engine,
a remote host accesible over SSH, a kubernetes cluster hosted in
a public cloud, among many other possibilities."))

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
	 (finalize-tenant-name-slot ()
	   (when (and (slot-boundp instance 'tenant)
		      (not (slot-boundp instance 'tenant-name)))
	     (with-slots (tenant tenant-name) instance
	       (setf (slot-value instance 'tenant-name)
		     (tenant-name tenant)))))
	 (finalize-project-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-boundp instance 'tenant))
	     (with-slots (project tenant) instance
	       (unless (typep project 'project)
		 (setf project (find-project project :tenant tenant))))))
	 (finalize-project-name-slot ()
	   (when (and (slot-boundp instance 'project)
		      (slot-value instance 'project)
		      (not (slot-boundp instance 'project-name)))
	     (with-slots (project) instance
	       (setf (slot-value instance 'project-name)
		     (project-name project))))))
    (initialize-tenant-slot-from-project-slot)
    (finalize-tenant-slot)
    (finalize-tenant-name-slot)
    (finalize-project-slot)
    (finalize-project-name-slot)))

(defun find-steward (designator &key tenant project steward-class)
  "The steward designated by DESIGNATOR.
When DESIGNATOR is a STEWARD, it is immediately returned. When
DESIGNATOR is a string, it is interpreted as a name to search
stewards in the given project."
  (flet ((check-tenant ()
	   (unless tenant
	     (error "A TENANT is required to find a steward.")))
	 (check-project ()
	   (unless project
	     (error "A PROJECT is required to find a steward.")))
	 (check-steward-class ()
	     (unless steward-class
	       (error "A STEWARD-CLASS is required to find a steward.")))
	 (return-early-if-tenant-does-not-exist ()
	   (when tenant
	     (setf tenant (find-tenant tenant)))
	   (unless tenant
	     (return-from find-steward nil)))
	 (return-early-if-project-does-not-exist ()
	   (when project
	     (setf project (find-project project :tenant tenant)))
	   (unless project
	     (return-from find-steward nil)))
	 (find-by-name (name)
	   (caar
	    (clsql:select
	     steward-class
 	     :where [and [= [tenant-name] (tenant-name tenant)]
                         [= [project-name] (project-name project)]
                         [= [name] name]]))))
    (etypecase designator
      (steward
       designator)
      (string
       (check-tenant)
       (check-project)
       (check-steward-class)
       (return-early-if-tenant-does-not-exist)
       (return-early-if-project-does-not-exist)
       (find-by-name designator))
      (null
       nil))))


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
