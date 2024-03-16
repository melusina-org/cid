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

(clsql:def-view-class steward (named-trait tenant-trait project-trait)
  ((description
    :type string
    :initarg :description
    :reader description
    :initform nil
    :documentation "A short description of the STEWARD."))
  (:documentation "The class represents stewards responsible for
resources consumed by the deployment of software components.

Some examples of STEWARDS are the localhost, a configured docker engine,
a remote host accesible over SSH, a kubernetes cluster hosted in
a public cloud, among many other possibilities."))

(defmethod address-components ((instance steward))
  '(tenant-name project-name))

(defun find-steward (designator &key tenant project steward-class)
  "The steward designated by DESIGNATOR.
When DESIGNATOR is a STEWARD, it is immediately returned. When DESIGNATOR
is a string, it is interpreted as a name to search stewards in the given
project. When DESIGNATOR is NIL, the returned value is also NIL."
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
 	     :where [and [= [tenant-name] (name tenant)]
                         [= [project-name] (name project)]
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
