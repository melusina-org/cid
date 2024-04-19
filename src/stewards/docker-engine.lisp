;;;; docker-engine.lisp — Docker-Engine Steward for El Cid

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

(defparameter *docker-client-pathname*
  #.(loop :for pathname
	  :in '(#p"/opt/local/bin/docker"
		#p"/usr/bin/docker"
		#p"/usr/local/bin/docker")
	  :when (probe-file pathname)
	  :return pathname)
  "The pathname to the docker engine client used to control resources.")

(clsql:def-view-class docker-engine (steward)
  ((steward-class
    :allocation :class
    :initform 'docker-engine
    :type symbol)
   (contains
    :db-kind :virtual
    :allocation :class
    :initform '(docker-image docker-container docker-volume))
   (description
    :allocation :class
    :initform
    "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."
    :type string)
   (pathname
    :type pathname
    :initarg :pathname
    :initform *docker-client-pathname*
    :documentation "The pathname to the docker engine client
used to control resources.")
   (version
    :type string
    :initarg :version
    :initform nil
    :documentation "The version information for the docker engine.
This version information is the text returned by the DOCKER VERSION command."))
  (:default-initargs
   :name "docker-engine"
   :displayname "Docker Engine")
  (:documentation
   "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."))

(defun make-docker-engine (&rest initargs &key tenant project name displayname pathname)
  "Make a docker-engine steward."
  (declare (ignore tenant project name displayname pathname))
  (apply #'make-instance 'docker-engine initargs))


;;;;
;;;; Docker Engine Command
;;;;

(defun run-docker-engine-query (docker-engine &rest argv)
  (uiop:run-program
   (cons (namestring (slot-value docker-engine 'pathname)) argv)
   :output :lines
   :error-output :lines))

(defun run-docker-engine-command (docker-engine &rest argv)
  (uiop:run-program
   (cons (namestring (slot-value docker-engine 'pathname)) argv)
   :output '(:string :stripped t)
   :error-output :lines))

(defun docker-version (docker-engine)
  (flet ((version (line)
	   (multiple-value-bind (match-start match-end
				 register-start register-end)
	       (ppcre:scan "^\\s*Version:\\s*(.*)" line)
	     (declare (ignore match-end))
	     (when match-start
	       (subseq line (aref register-start 0) (aref register-end 0))))))
  (loop :for line :in (run-docker-engine-query docker-engine "version")
	:for version = (version line) 
	:until version
	:finally (return version))))
		 

;;;;
;;;; Configure
;;;;

(defmethod configure-steward ((instance docker-engine))
  (with-slots (version) instance
    (unless version
      (setf version (docker-version instance))))
  (values nil))


;;;;
;;;; Docker Volume
;;;;

(clsql:def-view-class docker-volume (resource)
  ((steward-class
    :db-kind :virtual
    :type symbol
    :initform 'docker-engine
    :allocation :class)
   (volume
    :type string
    :initarg :volume
    :documentation "The volume name must consist of safe characters.")
   (driver
    :type string
    :initarg :driver
    :initform "local"))
  (:documentation
   "A local volume belonging to a docker engine steward."))

(defun make-docker-volume (&rest initargs &key docker-engine name displayname description
					       volume driver)
  "Make a local docker volume"
  (declare (ignore name displayname description volume driver))
  (check-type docker-engine docker-engine)
  (apply #'make-instance 'docker-volume
	 :steward docker-engine
	 (remove-property initargs :docker-engine)))

(defmethod examine-resource append ((instance docker-volume))
  (with-slots (volume driver) instance
    (list
     :volume volume
     :driver driver)))

(defun probe-docker-volume (steward volume)
  (flet ((make-properties (text)
	   (ppcre:register-groups-bind (volume driver) ("(.*)\\|(.*)" text)
	     (list
	      :volume volume
	      :driver driver)))
	 (inspect-volume ()
	   (run-docker-engine-command steward "volume" "inspect" volume "--format" "{{.Name}}|{{.Driver}}")))
    (handler-case (make-properties (inspect-volume))
      (uiop/run-program:subprocess-error (condition)
	(declare (ignore condition))
	(values nil)))))

(defmethod list-resource-identifiers ((steward docker-engine) (resource-class (eql 'docker-volume)))
  (nth-value 0 (run-docker-engine-query steward "volume" "list" "--format" "{{.Name}}")))

(defmethod list-resources ((steward docker-engine) (resource-class (eql 'docker-volume)))
  (flet ((make-docker-volume (text)
	   (ppcre:register-groups-bind (volume driver) ("(.*)\\|(.*)" text)
	     (let ((instance
		     (make-docker-volume :docker-engine steward
					 :volume volume
					 :driver driver)))
	       (with-slots (state identifier) instance
		 (setf state t
		       identifier volume))
	       (values instance))))
	 (query ()
	   (run-docker-engine-query steward "volume" "list" "--format" "{{.Name}}|{{.Driver}}")))
    (loop :for line :in (query)
	  :while line
	  :collect (make-docker-volume line))))

(defmethod update-instance-from-resource ((instance docker-volume))
  (flet ((ensure-volume-is-set-when-identifier-is-set ()
	   (when (and (slot-boundp instance 'identifier)
		      (slot-value instance 'identifier)
		      (not (slot-boundp instance 'volume)))
	     (setf (slot-value instance 'volume)
		   (slot-value instance 'identifier))))
	 (update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Docker volume no longer exists."))
	   (with-slots (volume driver state) instance
	     (setf volume (getf properties :volume)
		   driver (getf properties :driver)
		   state t))))
    (ensure-volume-is-set-when-identifier-is-set)
    (with-slots (steward volume) instance
      (update-instance (probe-docker-volume steward volume)))))

(defmethod create-resource ((instance docker-volume))
  (flet ((return-early-when-volume-already-exists (instance)
	   (with-slots (steward volume) instance
	     (when (probe-docker-volume steward volume)
	       (resource-error 'create-resource instance
			       "Docker volume already exists."
			       "There is already an existing docker volume under the name ~S
therefore the docker volume ~A with the same name cannot be created." volume instance))))
	 (create-docker-volume ()
	   (with-slots (steward volume driver) instance
	     (let ((created-volume
		     (run-docker-engine-command steward "volume" "create" "--driver" driver volume)))
	       (unless (string= created-volume volume)
		 (resource-error 'create-resource instance
				 "Docker volume cannot be created."
				 "Cannot create docker volume ~A" volume)))))
	 (update-identifier-and-state ()
	   (with-slots (volume identifier state) instance
	     (setf identifier volume
		   state t))))
    (return-early-when-volume-already-exists instance)
    (create-docker-volume)
    (update-identifier-and-state)))

(defmethod delete-resource ((instance docker-volume))
  (flet ((ensure-that-resource-still-exists ()
	   (with-slots (steward volume) instance
	     (unless (probe-docker-volume steward volume)
	       (resource-no-longer-exists
		'delete-resource instance
		"Docker volume no longer exists."))))
	 (delete-volume ()
	   (with-slots (steward driver volume) instance
	     (let ((deleted-volume
		     (run-docker-engine-command steward "volume" "rm" volume)))
	       (unless (string= deleted-volume volume)
		 (resource-error 'delete-resource instance
				 "Docker volume cannot be deleted."
				 "Cannot delete docker volume ~A" volume)))))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (ensure-that-resource-still-exists)
    (delete-volume)
    (update-state-and-identifier)))

;;;; End of file `docker-engine.lisp'
