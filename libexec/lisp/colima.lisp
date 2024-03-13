;;;; colima.lisp — Colima Support for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/colima
  (:use #:common-lisp)
  (:export
   #:instance
   #:instance-name
   #:instance-status
   #:instance-architecture
   #:instance-cpu-number
   #:instance-memory-size
   #:instance-disk-size
   
   #:list-instances
   #:find-instance
   #:start-instance
   #:stop-instance
   #:delete-instance))

(in-package #:org.melusina.cid/colima)

(defclass instance ()
  ((name
    :initarg :name
    :initform (error "A COLIMA instance requires a name.")
    :reader instance-name
    :documentation "The name of the instance.
This must a lowercase string.")
   (status
    :initarg :status
    :initform nil
    :reader instance-status)
   (architecture
    :initarg :architecture
    :initform :aarch64
    :reader instance-architecture
    :documentation "The architecture of the instance.
This can be one of the values :AARCH64 or :X86_64.")
   (cpu-number
    :initarg :cpu-number
    :initform 2
    :reader instance-cpu-number
    :documentation "The number of CPUs on the instance.")
   (memory-size
    :initarg :memory-size
    :initform 2
    :reader instance-memory-size
    :documentation "The size of instance memory in GB.")
   (disk-size
    :initarg :disk-size
    :initform 60
    :reader instance-disk-size
    :documentation "The size of instance disk storage in GB.")))

(defmethod print-object ((instance instance) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (name status) instance
      (format stream ":NAME ~S :STATUS ~S" name status))))

(defun architecture-of-string (string)
  (or
   (cdr
    (assoc
     string
     '(("aarch64" . :aarch64)
       ("x86_64" . :x86_64))
     :test #'string=))
   (error "The string ~S does not designate any instance architecture." string)))

(defun architecture-to-string (architecture)
  (ecase architecture
    (:aarch64 "aarch64")
    (:x86_64 "x86_64")))

(defun status-of-string (string)
  (or
   (cdr
    (assoc
     string
     '(("Stopped" . :stopped)
       ("Running" . :running))
     :test #'string=))
   (error "The string ~S does not designate any instance status." string)))

(defun instance-of-json (string)
  (let ((object
	  (yason:parse string))
	(1GB
	  #.(* 1024 1024 1024)))
    (flet ((get-field (field kind)
	     (multiple-value-bind (value present-p) (gethash field object)
	       (unless present-p
		 (error "Cannot read field ~A from JSON description of Colima instance." field))
	       (ecase kind
		 (:string
		  (string value))
		 (:memory-size
		  (round value 1GB))
		 (:integer
		  value)
		 (:architecture
		  (architecture-of-string value))
		 (:status
		  (status-of-string value))))))
      (make-instance
       'instance
       :name (get-field "name" :string)
       :status (get-field "status" :status)
       :architecture (get-field "arch" :architecture)
       :cpu-number (get-field "cpus" :integer)
       :memory-size (get-field "memory" :memory-size)
       :disk-size (get-field "disk" :memory-size)))))

(defun list-instances ()
  (with-input-from-string
      (colima-list
       (uiop:run-program
	'("colima" "list" "--json")
	:output :string))
    (loop :for line = (read-line colima-list nil nil)
	  :while line
	  :unless (string= "" line)
	  :collect (instance-of-json line))))

(defun find-instance (designator)
  "Find instance designated by DESIGNATOR."
  (typecase designator
    (instance     
     designator)
    (string
     (find designator (list-instances)
	   :key #'instance-name
	   :test #'string=))))

(defun ensure-instance-exists (instance)
  "Signal an error when INSTANCE does not exist."
  (unless (instance-status instance)
    (restart-case
	(error "Cannot operate on instance ~S as it has not been created."
	       (instance-name instance))
      (continue ()
	nil))))

(defun update-instance (instance)
  "Update INSTANCE slots from its actual state."
  (let ((actual-state
	  (find-instance (instance-name instance))))
    (unless actual-state
      (setf (slot-value instance 'status) nil)
      (return-from update-instance instance))
    (loop :for slot-name :in '(status architecture cpu-number memory-size disk-size)
	  :do (setf (slot-value instance slot-name)
		    (slot-value actual-state slot-name)))
    (values instance)))

(defun start-instance (instance)
  "Start an INSTANCE.
When the instance does not exist, it is created. The call is synchronous
and waits for the Colima instance to be ready before the control is given
back to the main program."
  (setf instance (find-instance instance))
  (with-slots (name architecture cpu-number memory-size disk-size)
      instance
    (uiop:run-program
     (list "colima" "start" name
	   "--vm-type" "vz"
	   "--arch" (architecture-to-string architecture)
	   "--cpu" (write-to-string cpu-number)
	   "--memory" (write-to-string memory-size)
	   "--disk" (write-to-string disk-size))
     :output t
     :error-output t))
  (update-instance instance))

(defun stop-instance (instance &optional force)
  "Stop an INSTANCE."
  (setf instance (find-instance instance))
  (ensure-instance-exists instance)
  (uiop:run-program
   (remove nil (list "colima" "stop" (when force "--force") (instance-name instance)))
   :output t
   :error-output t)
  (update-instance instance))

(defun delete-instance (instance)
  "Delete an INSTANCE."
  (setf instance (find-instance instance))
  (ensure-instance-exists instance)
  (unless (eq :stopped (instance-status instance))
    (restart-case (error "The instance to delete is not stopped.")
      (stop ()
	(stop-instance instance))))
  (uiop:run-program
   (list "colima" "delete" "--force" (instance-name instance))
   :output t
   :error-output t)
  (update-instance instance))

;;;; End of file `colima.lisp'
