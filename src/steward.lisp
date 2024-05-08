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


;;;;
;;;; Steward Directory
;;;;

(defparameter *steward-directory* nil
  "When set, this is a hashtable mapping keywords to stewards.")

(defun find-steward (designator)
  "Find steward in *STEWARD-DIRECTORY*."
  (etypecase designator
    (steward
     designator)
    (keyword
     (when *steward-directory*
       (nth-value 0 (gethash designator *steward-directory*))))
    (null
     nil)))


;;;;
;;;; Composite Steward
;;;;

(defclass composite-steward (steward)
  ((stewards
    :initarg :stewards
    :initform nil
    :documentation "A hash table whose keys are keywords and values are stewards
forming up the composite.

This slot can also be initialised from an alist or a plist."))
  (:documentation "A composite steward is a juxtaposition of stewards.
Such a steward can be used to create complex infrastructure stacks
that need to interact with several stewards."))

(defmethod initialize-instance :after ((instance composite-steward) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((support-initialize-stewards-slot-with-plist ()
	   (with-slots (stewards) instance
	     (when (plist-p stewards)
	       (setf stewards
		     (alexandria:plist-hash-table stewards :test #'eq)))))
	 (support-initialize-stewards-slot-with-alist ()
	   (with-slots (stewards) instance
	     (when (alist-p stewards)
	       (setf stewards
		     (alexandria:alist-hash-table stewards :test #'eq))))))
    (support-initialize-stewards-slot-with-plist)
    (support-initialize-stewards-slot-with-alist)))

(defun make-composite-steward (&rest initargs &key tenant project name displayname
						   description stewards)
  "Make a COMPOSITE-STEWARD."
  (declare (ignore tenant project name displayname description stewards))
  (apply #'make-instance 'composite-steward initargs))

(defmethod configure-steward ((instance composite-steward))
  (with-slots (stewards) instance
    (loop :for steward :in (alexandria:hash-table-values stewards)
	  :do (configure-steward steward))))

(defmethod persistent-constructor ((class (eql 'composite-steward)))
  #'make-composite-steward)

(defmethod persistent-slots append ((instance composite-steward))
  '((:stewards stewards :presentation #'alexandria:hash-table-plist)))

(defmacro with-composite-steward-directory ((composite-steward) &body body)
  "Run BODY forms in an environment where COMPOSITE-STEWARD is used as a directory."
  `(let ((*steward-directory*
	   (slot-value ,composite-steward 'stewards)))
     ,@body))

;;;; End of file `steward.lisp'
