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

(in-package #:org.melusina.cid)

(defparameter *colima-tool-pathname*
  #.(loop :for pathname
	  :in '(#p"/opt/local/bin/colima")
	  :when (probe-file pathname)
	  :return pathname)
  "The pathname to the colima tool used to control resources.")

(defclass colima-tool (steward)
    ((pathname
      :type pathname
      :initarg :pathname
      :initform *colima-tool-pathname*
      :documentation "The pathname to the colima tool client used to control resources.")
     (tool-version
      :type (or string null)
      :initarg :tool-version
      :initform nil
      :documentation "The client version information for the colima tool.
This version information is the text returned by the COLIMA VERSION command."))
  (:default-initargs
   :name "colima-tool"
   :displayname "Colima Tool"
   :description
   "The class represents a provider creating resources in a colima tool.
The target colima tool is just the colima tool configured for the `colima'
CLI client.")
  (:documentation
   "The class represents a provider creating resources in a colima tool.
The target colima tool is just the  colima tool configured for
the `colima' CLI client."))

(defun make-colima-tool (&rest initargs &key tenant project name displayname description pathname tool-version)
  "Make a colima-tool steward."
  (declare (ignore tenant project name displayname description pathname tool-version))
  (apply #'make-instance 'colima-tool initargs))

(defmethod persistent-constructor ((class (eql 'colima-tool)))
  'make-colima-tool)

(defmethod persistent-slots append ((instance colima-tool))
  '((:initarg :pathname
     :slot-name pathname)
    (:initarg :tool-version
     :slot-name tool-version)))


;;;;
;;;; Colima Tool Query and Command
;;;;

(defun run-colima-tool-query (colima-tool &rest argv)
  "Run COMMAND as a query to the Colima Tool and return the resulting lines."
  (uiop:run-program
   (cons (namestring (slot-value colima-tool 'pathname))
	 (loop :for arg :in argv
	       :append (alexandria:ensure-list arg)))
   :output :lines
   :error-output :lines))

(defun run-colima-tool-command (colima-tool &rest argv)
  "Run COMMAND as a command to the Colima Tool and return the resulting output as a stripped string."
  (uiop:run-program 
   (cons (namestring (slot-value colima-tool 'pathname))
	 (loop :for arg :in argv
	       :append (alexandria:ensure-list arg)))
   :output '(:string :stripped t)
   :error-output :lines
   :ignore-error-status t))


;;;;
;;;; Configure
;;;;

(defmethod configure-steward ((instance colima-tool))
  (flet ((scan-version (text)
	   (let ((version-re
		   (ppcre:parse-string "v[0-9]+[.][0-9]+[.][0-9]+")))
	     (multiple-value-bind (match-start match-end) (ppcre:scan version-re text)
	       (when match-start
		 (subseq text match-start match-end)))))
	 (store-version (version)
	   (when version
	     (with-slots (tool-version) instance
	       (setf tool-version version)))))
    (store-version (scan-version (run-colima-tool-command instance "version")))))


;;;;
;;;; Colima Instance
;;;;

(defclass colima-instance (resource)
  ((steward-class
    :type symbol
    :initform 'colima-tool
    :allocation :class)
   (profile
    :type string
    :initarg :profile
    :reader colima-instance-profile)
   (architecture
    :initarg :architecture
    :initform :aarch64
    :reader colima-instance-architecture
    :documentation "The architecture of the instance.
This can be one of the values :AARCH64 or :X86_64.")
   (cpu-number
    :initarg :cpu-number
    :initform 2
    :reader colima-instance-cpu-number
    :documentation "The number of CPUs on the instance.")
   (memory-size
    :initarg :memory-size
    :initform 2
    :reader colima-instance-memory-size
    :documentation "The size of instance memory in GB.")
   (disk-size
    :initarg :disk-size
    :initform 60
    :reader colima-instance-disk-size
    :documentation "The size of instance disk storage in GB.")))

(defun make-colima-instance (&rest initargs &key colima-tool name displayname description
						 state identifier parent external
						 profile architecture cpu-number memory-size disk-size)
  "Make a COLIMA-INSTANCE"
  (declare (ignore name displayname description
		   state identifier parent external
		   profile architecture cpu-number memory-size disk-size))
  (check-type colima-tool colima-tool)
  (apply #'make-instance 'colima-instance
	 :steward colima-tool
	 (remove-property initargs :colima-tool)))

(defmethod persistent-constructor ((class (eql 'colima-instance)))
  'make-colima-instance)

(defmethod persistent-slots append ((instance colima-instance))
  '((:initarg :colima-tool
     :slot-name steward)
    (:initarg :profile
     :slot-name profile
     :immutable t)
    (:initarg :architecture
     :slot-name architecture
     :immutable t)
    (:initarg :cpu-number
     :slot-name cpu-number
     :immutable t)
    (:initarg :memory-size
     :slot-name memory-size
     :immutable t)
    (:initarg :disk-size
     :slot-name disk-size
     :immutable t)))

(defmethod examine-resource append ((instance colima-instance))
  (with-slots (profile architecture cpu-number memory-size disk-size) instance
    (list
     :profile profile
     :architecture architecture
     :cpu-number cpu-number
     :memory-size memory-size
     :disk-size disk-size)))

(defmethod resource-ready-p ((instance colima-instance))
  (eq :running (slot-value instance 'state)))

(defmethod list-resources ((steward colima-tool)
			   (resource-class (eql 'colima-instance)))
  (flet ((colima-instance-of-json (string)
	   (let ((object
		   (yason:parse string))
		 (1GB
		   #.(* 1024 1024 1024)))
	     (labels ((get-field (field kind)
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
			     (status-of-string value)))))
		      (architecture-of-string (string)
			(or
			 (cdr
			  (assoc
			   string
			   '(("aarch64" . :aarch64)
			     ("x86_64" . :x86_64))
			   :test #'string=))
			 (error "The string ~S does not designate any instance architecture." string)))
		      (status-of-string (string)
			(or
			 (cdr
			  (assoc
			   string
			   '(("Stopped" . :stopped)
			     ("Running" . :running))
			   :test #'string=))
			 (error "The string ~S does not designate any instance status." string))))
	       (make-colima-instance
		:colima-tool steward
		:name (get-field "name" :string)
		:profile (get-field "name" :string)
		:identifier (get-field "name" :string)
		:state (get-field "status" :status)
		:architecture (get-field "arch" :architecture)
		:cpu-number (get-field "cpus" :integer)
		:memory-size (get-field "memory" :memory-size)
		:disk-size (get-field "disk" :memory-size))))))
    (loop :for line in (run-colima-tool-query steward "list" "--json")
	  :unless (string= "" line)
	  :collect (colima-instance-of-json line))))

(defmethod list-resource-identifiers ((steward colima-tool)
				      (resource-class (eql 'colima-instance)))
  (loop :for instance :in (list-resources steward resource-class)
	:collect (resource-identifier instance)))


(defmethod update-instance-from-resource ((instance colima-instance))
  (flet ((update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Colima instance no longer exists."))
	   (with-slots (profile architecture cpu-number memory-size disk-size state) instance
	     (setf profile (getf properties :profile)
		   architecture (getf properties :architecture)
		   cpu-number (getf properties :cpu-number)
		   memory-size (getf properties :memory-size)
		   disk-size (getf properties :disk-size)
		   state (getf properties :state))))
	 (examine-resource (instance)
	   (when instance
	     (examine-resource instance)))
	 (actual-resource (instance)
	   (find (slot-value instance 'identifier)
		 (list-resources (steward instance)
				 'colima-instance)
		 :key #'resource-identifier
		 :test #'string=)))
    (update-instance
     (examine-resource
      (actual-resource instance)))))

(defmethod create-resource ((instance colima-instance))
  (flet ((return-early-when-instance-already-exists (instance)
	   (with-slots (steward profile) instance
	     (when (find profile
			 (list-resources steward 'colima-instance)
			 :key #'resource-identifier
			 :test #'string=)
	       (resource-error 'create-resource instance
			       "Colima instance already exists."
			       "There is already an existing Colima instance under the name ~S
therefore the Colima instance ~A with the same name cannot be created." profile instance))))
	 (create-colima-instance ()
	   (with-slots (profile architecture cpu-number memory-size disk-size)
	       instance
	     (flet ((architecture-to-string (architecture)
		      (ecase architecture
			(:aarch64 "aarch64")
			(:x86_64 "x86_64"))))
	       (uiop:run-program
		(list "colima" "start" profile
		      "--activate=false"
		      "--vm-type" "vz"
		      "--arch" (architecture-to-string architecture)
		      "--cpu" (write-to-string cpu-number)
		      "--memory" (write-to-string memory-size)
		      "--disk" (write-to-string disk-size))
		:output t
		:error-output t))))
	 (update-identifier-and-state ()
	   (with-slots (profile identifier state) instance
	     (setf identifier profile
		   state :running))))
    (return-early-when-instance-already-exists instance)
    (create-colima-instance)
    (update-identifier-and-state)))

(defmethod delete-resource ((instance colima-instance))
  (flet ((ensure-that-instance-still-exists ()
	   (with-slots (steward profile) instance
	     (unless (find profile
			   (list-resources steward 'colima-instance)
			   :key #'resource-identifier
			   :test #'string=)
	       (resource-no-longer-exists
		'delete-resource instance
		"Colima instance no longer exists."))))
	 (delete-instance ()
	   (with-slots (steward profile) instance
	     (run-colima-tool-command
	      steward
	      "delete" "--force" profile)))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (ensure-that-instance-still-exists)
    (delete-instance)
    (update-state-and-identifier)))

(defmethod update-resource-from-instance ((instance colima-instance))
  nil)

;;;; End of file `colima.lisp'
