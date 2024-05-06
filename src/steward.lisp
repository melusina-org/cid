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
    :initform *tenant*)
   (project
    :type project
    :reader project
    :initarg :project
    :initform *project*)
   (description
    :type (or string null)
    :initarg :description
    :reader description
    :initform nil
    :documentation "A short description of the STEWARD."))
  (:documentation "The class represents stewards responsible for
resources consumed by the deployment of software components.

Some examples of STEWARDS are the localhost, a configured docker engine,
a remote host accesible over SSH, a kubernetes cluster hosted in
a public cloud, among many other possibilities."))

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

(defmethod readable-slots append ((instance steward))
  '((:tenant tenant)
    (:project project)
    (:description description)))

(defmethod print-object ((instance steward) stream)
  (flet ((print-readably ()
	   (print-readable-object instance stream))
	 (print-unreadably ()
	   (with-slots (tenant project name displayname) instance
	     (print-unreadable-object (instance stream :type t :identity t)
	       (format stream "~A:~A:~A ~A" (name tenant) (name project) name displayname)))))
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
