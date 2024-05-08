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

(defclass steward (named-trait)
  ((tenant
    :type tenant
    :reader tenant
    :initarg :tenant
    :initform *tenant*
    :documentation "The TENANT this STEWARD operates for.")
   (project
    :type project
    :reader project
    :initarg :project
    :initform *project*
    :documentation "The PROJECT this STEWARD operates for.")
   (description
    :type (or string null)
    :initarg :description
    :reader description
    :initform nil
    :documentation "A short description of the STEWARD."))
  (:documentation "The class represents stewards responsible for resources
that are examined, created, deleted and consumed during the deployment
of infrastructure stacks."))

(defmethod initialize-instance :after ((instance steward) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((support-initialize-tenant-slot-with-designator ()
	   (cond
	     ((typep (slot-value instance 'tenant) 'string)
	      (with-slots (tenant) instance
		(setf tenant (or (find-tenant tenant)
				 (error "Cannot find tenant ~S." tenant)))))))
	 (support-initialize-project-slot-with-designator ()
	   (cond
	     ((typep (slot-value instance 'project) 'string)
	      (with-slots (tenant project) instance
		(setf project (or (find-project project :tenant tenant )
				  (error "Cannot find project ~S for tenant ~S."
					 project (name tenant)))))))))
    (support-initialize-tenant-slot-with-designator)
    (support-initialize-project-slot-with-designator)))

(defmethod persistent-slots append ((instance steward))
  '((:tenant tenant)
    (:project project)
    (:description description)))

(defmethod print-object ((instance steward) stream)
  (flet ((print-readably ()
	   (write-persistent-object instance stream))
	 (print-unreadably ()
	   (flet ((print-scope ()
		    (format stream "~A:~A"
			    (name (tenant instance))
			    (name (project instance))))
		  (print-name ()
		    (with-slots (name displayname) instance
		      (when (and name displayname)
			(format stream ":~A ~A" name displayname)))))
	     (print-unreadable-object (instance stream :type t :identity t)
	       (print-scope)
	       (print-name)))))
    (if *print-readably*
	(print-readably)
	(print-unreadably))))

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
