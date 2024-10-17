;;;; simulator.lisp — Simulator Steward for El Cid

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


;;;;
;;;; Simulator
;;;;

(defclass simulator (steward)
  ((description
    :initform "A steward that does not administrate actual resources."
    :type string)
   (resource-identifiers
    :initarg :resource-identifiers
    :initform nil
    :documentation "The list of resource identifiers that have been created."))
  (:default-initargs
   :name "simulator"
   :displayname "Resource Simulator")
  (:documentation
   "A steward that does not administrate actual resources.
A SIMULATOR stewards creates SIMULATION resources. These resources do not
have underlying resources but otherwise support all operations that a regular
resource supports.  It is possible to use a SIMULATOR and SIMULATION resources
to test lifecycles and invariants, and as a draft when implementing
new resources."))

(defun make-simulator (&rest initargs &key tenant project name displayname description resource-identifiers)
  "Make a SIMULATOR with the given parameters."
  (declare (ignore tenant project name displayname description resource-identifiers))
  (apply #'make-instance 'simulator initargs))

(defmethod persistent-constructor ((class (eql 'simulator)))
  'make-simulator)

(defmethod persistent-slots append ((instance simulator))
  '((:initarg :resource-identifiers
     :slot-name resource-identifiers)))


;;;;
;;;; Simulation
;;;;

(defclass simulation (resource)
  ((steward-class
    :type symbol
    :initform 'simulator
    :allocation :class))
  (:documentation
   "A resource simulation, every step of the lifecycle is a no-operation."))

(defun make-simulation (&rest initargs
			&key simulator name displayname description 
			     state identifier parent external)
  "Make a simulator resource."
  (declare (ignore name displayname description state identifier parent external))
  (apply #'make-instance 'simulation
	 :steward simulator
	 (remove-property initargs :simulator)))

(defmethod persistent-constructor ((class (eql 'simulation)))
  'make-simulation)

(defmethod create-resource ((instance simulation))
  (with-slots (steward identifier state) instance
    (with-slots (resource-identifiers) steward
      (when (and identifier
		 (member identifier resource-identifiers :test #'string=))
	(resource-error
	 'create-resource instance
	 "Cannot create simulator resource, resource already exists."
	 "It is not possible for steward ~A to create the simulator resource ~A.
This resource already exists."
	 steward instance))
      (setf identifier (random-string 7)
	    state t)
      (push identifier resource-identifiers))))

(defmethod delete-resource ((instance simulation))
  (with-slots (steward identifier state delete-error-p) instance
    (with-slots (resource-identifiers) steward
      (unless (member identifier resource-identifiers :test #'string=)
	(resource-no-longer-exists
	 'delete-resource instance
	 "Cannot delete simulator resource, resource does not exist."
	 "It is not possible for steward ~A to delete the simulator resource ~A.
This resource does not actually exist."
	 steward instance))
      (setf resource-identifiers
	    (delete identifier resource-identifiers :test #'string=)
	    identifier nil
	    state nil))))

(defmethod list-resource-identifiers ((steward simulator) (resource-class (eql 'simulation)))
  (slot-value steward 'resource-identifiers))

(defmethod update-instance-from-resource ((instance simulation))
  (with-slots (state identifier) instance
    (if (member identifier (list-resource-identifiers (steward instance) 'simulation)
		:test #'string=)
	(setf state t)
	(setf state nil
	      identifier nil))))

(defmethod update-resource-from-instance ((instance simulation))
  (values))

;;;; End of file `simulator.lisp'
