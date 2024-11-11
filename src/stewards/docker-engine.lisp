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

(defclass docker-engine (steward)
    ((pathname
      :type pathname
      :initarg :pathname
      :initform *docker-client-pathname*
      :documentation "The pathname to the docker engine client used to control
    resources.")
     (context
      :type string
      :initarg :context
      :initform "default"
      :documentation "The docker context to use.")
     (client-version
      :type (or string null)
      :initarg :client-version
      :initform nil
      :documentation "The client version information for the docker engine.
This version information is the text returned by the DOCKER VERSION command.")
     (server-version
      :type (or string null)
      :initarg :server-version
      :initform nil
      :documentation "The server version information for the docker engine.
This version information is the text returned by the DOCKER VERSION command."))
  (:default-initargs
   :name "docker-engine"
   :displayname "Docker Engine"
   :description
   "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client.")
  (:documentation
   "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."))

(defun make-docker-engine (&rest initargs &key tenant project name displayname description pathname context client-version server-version)
  "Make a docker-engine steward."
  (declare (ignore tenant project name displayname description pathname context client-version server-version))
  (apply #'make-instance 'docker-engine initargs))

(defmethod persistent-constructor ((class (eql 'docker-engine)))
  'make-docker-engine)

(defmethod persistent-slots append ((instance docker-engine))
  '((:initarg :pathname
     :slot-name pathname)
    (:initarg :context
     :slot-name context)
    (:initarg :client-version
     :slot-name client-version)
    (:initarg :server-version
     :slot-name server-version)))


;;;;
;;;; Docker Engine Query and Command
;;;;

(defun run-docker-engine-query (docker-engine &rest argv)
  "Run COMMAND as a query to the docker engine and return the resulting lines."
  (uiop:run-program
   (list*
    (namestring (slot-value docker-engine 'pathname))
    "--context" (slot-value docker-engine 'context)
    (loop :for arg :in argv
	  :append (alexandria:ensure-list arg)))
   :output :lines
   :error-output :lines))

(defun run-docker-engine-command (docker-engine &rest argv)
  (uiop:run-program 
   (list*
    (namestring (slot-value docker-engine 'pathname))
    "--context" (slot-value docker-engine 'context)
    (loop :for arg :in argv
	  :append (alexandria:ensure-list arg)))
   :output '(:string :stripped t)
   :error-output :lines
   :ignore-error-status t))


;;;;
;;;; Configure
;;;;

(defun docker-engine-version (docker-engine)
  (extract-json-fields
   (run-docker-engine-command docker-engine "version" "--format"
			      "{\"client\":\"{{ .Client.Version }}\",\"server\":\"{{ .Server.Version }}\"}")
   '((:initarg :client
      :json-name "client"
      :type string)
     (:initarg :server
      :json-name "server"
      :type string))))

(defmethod configure-steward ((instance docker-engine))
  (with-slots (client-version server-version) instance
    (let ((version-information
	    (docker-engine-version instance)))
      (setf client-version (getf version-information :client)
	    server-version (getf version-information :server)))))

(defun docker-engine-context (docker-engine)
  (multiple-value-bind (output error-output exit-code)
      (run-docker-engine-command docker-engine "version" "--format"
				 "{{ .Client.Context }}")
    (cond ((= 0 exit-code)
	   (values output))
	  (t
	   (error error-output)))))


;;;;
;;;; Docker Volume
;;;;

(defclass docker-volume (resource)
  ((steward-class
    :type symbol
    :initform 'docker-engine
    :allocation :class)
   (volume
    :type string
    :initarg :volume)
   (driver
    :type string
    :initarg :driver
    :initform "local"))
  (:documentation
   "A local volume belonging to a docker engine steward."))

(defun make-docker-volume (&rest initargs &key docker-engine name displayname description
					       state identifier parent external
					       volume driver)
  "Make a local docker volume"
  (declare (ignore name displayname description
		   state identifier parent external
		   volume driver))
  (check-type docker-engine docker-engine)
  (apply #'make-instance 'docker-volume
	 :steward docker-engine
	 (remove-property initargs :docker-engine)))

(defmethod persistent-constructor ((class (eql 'docker-volume)))
  'make-docker-volume)

(defmethod persistent-slots append ((instance docker-volume))
  '((:initarg :docker-engine
     :slot-name steward)
    (:initarg :volume
     :slot-name volume
     :immutable t)
    (:initarg :driver
     :slot-name driver
     :immutable t)))

(defmethod examine-resource append ((instance docker-volume))
  (with-slots (volume driver) instance
    (list
     :volume volume
     :driver driver)))

(defun probe-docker-volume (steward volume)
  (flet ((extract-json-fields (text)
	   (extract-json-fields
	    text
	    '((:initarg :volume
	       :json-name "Name"
	       :type string)
	      (:initarg :driver
	       :json-name "Driver"
	       :type string))))
	 (inspect-volume ()
	   (multiple-value-bind (output error-output exit-code)
	       (run-docker-engine-command steward "volume" "inspect" "--format" "{{ json . }}" volume)
	     (cond
	       ((= 0 exit-code)
		(values output))
	       (t
		(error error-output))))))
    (handler-case (extract-json-fields (inspect-volume))
      (error (condition)
	(declare (ignore condition))
	(values nil)))))

(defmethod list-resource-identifiers ((steward docker-engine) (resource-class (eql 'docker-volume)))
  (nth-value 0 (run-docker-engine-query steward "volume" "list" "--format" "{{ .Name }}")))

(defmethod list-resources ((steward docker-engine) (resource-class (eql 'docker-volume)))
  (flet ((make-docker-volume (text)
	   (let ((properties
		   (extract-json-fields
		    text
		    '((:initarg :identifier
		       :json-name "Name"
		       :type string)
		      (:initarg :volume
		       :json-name "Name"
		       :type string)
		      (:initarg :driver
		       :json-name "Driver"
		       :type string)))))
	     (apply #'make-docker-volume
		    :docker-engine steward
		    :state t
		    properties)))
	 (query ()
	   (run-docker-engine-query steward "volume" "list" "--format" "json")))
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

(defmethod update-resource-from-instance ((instance docker-volume))
  nil)


;;;;
;;;; Docker Compose Project
;;;;

(defclass docker-project (resource)
  ((steward-class
    :type symbol
    :initform 'docker-engine
    :allocation :class)
   (project
    :type string
    :initarg :project)
   (pathname
    :type pathname
    :initarg :pathname)
   (volumes
    :type list
    :initarg :volumes
    :initform nil
    :documentation
    "The list of external volumes which are consumed by the docker project.")
   (environment
    :type list
    :initarg :environment
    :initform nil
    :documentation
    "The environment variables used for the compose file."))
  (:documentation
   "A docker compose project belonging to a docker engine steward."))

(defun make-docker-project (&rest initargs &key docker-engine name displayname description
						state identifier parent external
						project pathname volumes environment)
  "Make a docker compose project."
  (declare (ignore name displayname description
		   state identifier parent external
		   project pathname volumes environment))
  (check-type docker-engine docker-engine)
  (apply #'make-instance 'docker-project
	 :steward docker-engine
	 (remove-property initargs :docker-engine)))

(defmethod persistent-constructor ((class (eql 'docker-project)))
  'make-docker-project)

(defmethod persistent-slots append ((instance docker-project))
  '((:initarg :docker-engine
     :slot-name steward)
    (:initarg :project
     :slot-name project
     :immutable t)
    (:initarg :pathname
     :slot-name pathname
     :immutable t)
    (:initarg :volumes
     :slot-name volumes
     :immutable t)
    (:initarg :environment
     :slot-name environment
     :immutable t)))

(defmethod examine-resource append ((instance docker-project))
  (with-slots (project pathname) instance
    (list
     :project project
     :pathname pathname)))

(defmethod resource-prerequisites append ((instance docker-project))
  (slot-value instance 'volumes))

(defun docker-project-status (string)
  (flet ((status-name ()
	   (ppcre:register-groups-bind (name) ("([a-zA-Z]+)\\([0-9]+\\)" string)
	     (values name)))
	 (make-keyword (name)
	   (when (member name '("running" "stopped" "starting" "restarting")
			 :test #'string=)
	     (alexandria:make-keyword (string-upcase name))))
	 (status-to-state (status)
	   (ecase status
	     (:running
	      t)
	     ((:stopped :starting :restarting)
	      status))))
    (status-to-state (make-keyword (status-name)))))

(defun probe-docker-projects (steward)
  (extract-json-fields
   (run-docker-engine-command steward "compose" "ls" "--format" "json")
   `((:initarg :project
      :json-name "Name"
      :type string)
     (:initarg :state
      :json-name "Status"
      :type string
      :key ,#'docker-project-status)
     (:initarg :pathname
      :json-name "ConfigFiles"
      :type string
      :key ,#'pathname))))

(defun probe-docker-project (steward name)
  (flet ((project-name (plist)
	   (getf plist :project)))
    (find name (probe-docker-projects steward)
	  :test #'string=
	  :key #'project-name)))

(defmacro with-project-environment (project &body body)
  (alexandria:once-only (project)
    `(with-environment (slot-value ,project 'environment)
       ,@body)))

(defmethod list-resource-identifiers ((steward docker-engine) (resource-class (eql 'docker-project)))
  (flet ((project-name (plist)
	   (getf plist :project)))
    (mapcar #'project-name (probe-docker-projects steward))))

(defmethod list-resources ((steward docker-engine) (resource-class (eql 'docker-project)))
  (loop :for properties :in (probe-docker-projects steward)
	:collect (apply #'make-docker-project
			:docker-engine steward
			:identifier (getf properties :project)
			:state t
			properties)))

(defmethod update-instance-from-resource ((instance docker-project))
  (flet ((ensure-project-is-set-when-identifier-is-set ()
	   (when (and (slot-boundp instance 'identifier)
		      (slot-value instance 'identifier)
		      (not (slot-boundp instance 'project)))
	     (setf (slot-value instance 'project)
		   (slot-value instance 'identifier))))
	 (update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Docker compose project no longer exists."))
	   (with-slots (project pathname state) instance
	     (setf project (getf properties :project)
		   pathname (getf properties :pathname)
		   state t))))
    (ensure-project-is-set-when-identifier-is-set)
    (with-slots (steward project) instance
      (update-instance (probe-docker-project steward project)))))

(defmethod create-resource ((instance docker-project))
  (flet ((return-early-when-project-already-exists (instance)
	   (with-slots (steward project) instance
	     (when (probe-docker-project steward project)
	       (resource-error 'create-resource instance
			       "Docker project already exists."
			       "There is already an existing docker compose project under the name ~S
therefore the docker project ~A with the same name cannot be created." project instance))))
	 (create-docker-project ()
	   (with-slots (steward project pathname) instance
	     (with-project-environment instance
	       (multiple-value-bind (output error-output exit-code)
		   (run-docker-engine-command steward "compose"
					      "--project-name" project
					      "--file" (namestring pathname)
					      "up"
					      "--detach"
					      "--wait")
		 (cond
		   ((= 0 exit-code)
		    (values output))
		   (t
		    (resource-error 'create-resource instance
				    "Docker project cannot be created."
				    "Cannot create docker project ~A
the docker compose command terminated with code ~A and provided the
diagnostics

~A

and

~A"
				    project exit-code output error-output)))))))
	 (update-identifier-and-state ()
	   (with-slots (project identifier state) instance
	     (setf identifier project
		   state t))))
    (return-early-when-project-already-exists instance)
    (create-docker-project)
    (update-identifier-and-state)))

(defmethod delete-resource ((instance docker-project))
  (flet ((ensure-that-resource-still-exists ()
	   (with-slots (steward project) instance
	     (unless (probe-docker-project steward project)
	       (resource-no-longer-exists
		'delete-resource instance
		"Docker project no longer exists."))))
	 (delete-project ()
	   (with-slots (steward pathname project) instance
	     (with-project-environment instance
	       (multiple-value-bind (output error-output exit-code)
		   (run-docker-engine-command steward "compose"
					      "--project-name" project
					      "--file" (namestring pathname)
					      "down")
		 (cond
		   ((= 0 exit-code)
		    (values output))
		   (t
		    (resource-error 'delete-resource instance
				    "Docker project cannot be deleted."
				    "Cannot delete docker project ~A
the docker compose command terminated with code ~A and provided the
diagnostics

~A

and

~A"
				    project exit-code output error-output)))))))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (ensure-that-resource-still-exists)
    (delete-project)
    (update-state-and-identifier)))

(defmethod update-resource-from-instance ((instance docker-project))
  nil)

;;;; End of file `docker-engine.lisp'
