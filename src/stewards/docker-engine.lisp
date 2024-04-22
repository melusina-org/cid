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
  (flet ((accept-lists-in-argv ()
	   (setf argv
		 (loop :for arg :in argv
		       :append (alexandria:ensure-list arg)))))
    (accept-lists-in-argv)
    (uiop:run-program
     (cons (namestring (slot-value docker-engine 'pathname)) argv)
     :output '(:string :stripped t)
     :error-output :lines)))

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
  (flet ((extract-json-fields (text)
	   (extract-json-fields
	    text
	    '((:property :volume
	       :name "Name"
	       :type string)
	      (:property :driver
	       :name "Driver"
	       :type string))))
	 (inspect-volume ()
	   (run-docker-engine-command steward "volume" "inspect" volume "--format" "{{json .}}")))
    (handler-case (extract-json-fields (inspect-volume))
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


;;;;
;;;; Docker Image
;;;;

(clsql:def-view-class docker-image (resource)
  ((steward-class
    :db-kind :virtual
    :type symbol
    :initform 'docker-engine
    :allocation :class)
   (id
    :initarg :id
    :initform nil
    :type string
    :reader image-id)
   (repository
    :initarg :repository
    :initform nil
    :type string
    :reader image-repository)
   (tag
    :initarg :tag
    :initform nil
    :type string
    :reader image-tag)
   (size
    :initarg :size
    :initform nil
    :type string
    :reader image-size)
   (created
    :initarg :created
    :type string
    :initform nil
    :reader image-created)
   (dockerfile
    :initarg :dockerfile
    :type pathname
    :initform nil)
   (context
    :initarg :context
    :type pathname
    :initform nil)
   (cache
    :initarg :cache
    :type boolean
    :initform t)
   (build-time-variables
    :db-type :virtual
    :initarg :build-time-variables
    :initform nil)))

(defun make-docker-image (&rest initargs &key docker-engine name displayname description
					      id repository tag size created
					      dockerfile context cache
					      build-time-variables)
  "Make a docker image"
  (declare (ignore name displayname description id repository tag size created
		   dockerfile context cache build-time-variables))
  (check-type docker-engine docker-engine)
  (apply #'make-instance 'docker-image
	 :steward docker-engine
	 (remove-property initargs :docker-engine)))

(defmethod print-object ((instance docker-image) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (repository tag size created) instance
      (format stream
	      "~@[:REPOSITORY ~S ~]~@[:TAG ~S ~]~@[:SIZE ~S ~]~@[:CREATED ~S~]"
	      repository tag size created))))

(defun probe-docker-image (steward id)
  (flet ((extract-json-fields (text)
	   (flet ((extract-repository (repo-tags)
		    (when repo-tags
		      (subseq (first repo-tags) 0 (position #\: (first repo-tags)))))
		  (extract-tag (repo-tags)
		    (when repo-tags
		      (subseq (first repo-tags) (1+ (position #\: (first repo-tags)))))))
	     (extract-json-fields
	      text
	      `((:property :id
		 :name "Id"
		 :type string)
		(:property :repository
		 :name "RepoTags"
		 :type (list string)
		 :key ,#'extract-repository)
		(:property :tag
		 :name "RepoTags"
		 :type (list string)
		 :key ,#'extract-tag)
		(:property :size
		 :name "Size"
		 :type integer)
		(:property :created
		 :name "Created"
		 :type string)))))
	 (inspect-image ()
	   (run-docker-engine-command steward "image" "inspect" id "--format" "{{json .}}")))
    (handler-case (extract-json-fields (inspect-image))
      (uiop/run-program:subprocess-error (condition)
	(declare (ignore condition))
	(values nil)))))

(defun image-name (image)
  (with-slots (repository tag) image
    (when (and repository tag)
      (concatenate 'string repository '(#\:) tag))))

(defun image-short-id (image)
  (with-slots (id) image
    (when id
      (let ((start
	      (1+ (position #\: id)))
	    (length
	      12))
      (subseq id start (+ start length))))))

(defmethod list-resource-identifiers ((steward docker-engine) (resource-class (eql 'docker-image)))
  (flet ((unique-identifiers (list)
	   (remove-duplicates list :test #'string=)))
    (unique-identifiers (run-docker-engine-query steward "image" "list""--no-trunc" "--format" "{{.ID}}"))))

(defmethod list-resources ((steward docker-engine) (resource-class (eql 'docker-image)))
  (flet ((docker-image-of-json (text)
	   (apply
	    #'make-docker-image
	    :docker-engine steward
	    :name "docker-image-name"
	    :displayname "Docker Image Name"
	    :description "A docker image."
	    (extract-json-fields
	     text
	     '((:property :id
		:name "ID"
		:type string)
	       (:property :repository
		:name "Repository"
		:type (or string null))
	       (:property :tag
		:name "Tag"
		:type (or string null))
	       (:property :size
		:name "Size"
		:type string)
	       (:property :created
		:name "CreatedAt"
		:type string)))))
	 (update-state-and-identifier (instance)
	   (with-slots (id identifier state) instance
	     (setf state t
		   identifier id))
	   (values instance))
	 (list-docker-images ()
	   (run-docker-engine-query steward "image" "list" "--no-trunc" "--format" "{{json .}}")))
    (loop :for line :in (list-docker-images)
	  :while line
	  :collect (update-state-and-identifier (docker-image-of-json line)))))

(defmethod update-instance-from-resource ((instance docker-image))
  (flet ((ensure-id-is-set-when-identifier-is-set ()
	   (when (and (slot-boundp instance 'identifier)
		      (slot-value instance 'identifier)
		      (not (slot-value instance 'id)))
	     (setf (slot-value instance 'id)
		   (slot-value instance 'identifier))))
	 (update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Docker image no longer exists."))
	   (with-slots (state id repository tag size created) instance
	     (setf id (getf properties :id)
		   repository (getf properties :repository)
		   tag (getf properties :tag)
		   size (getf properties :size)
		   created (getf properties :created)
		   state t))))
    (ensure-id-is-set-when-identifier-is-set)
    (with-slots (steward id) instance
      (update-instance (probe-docker-image steward id)))))

(defmethod create-resource ((instance docker-image))
  (flet ((return-early-when-image-already-exists (instance)
	   (with-slots (steward id) instance
	     (when (probe-docker-image steward id)
	       (resource-error 'create-resource instance
			       "Docker image already exists."
			       "There is already an existing docker image under the name ~S
therefore the docker image ~A with the same name cannot be created." (image-name instance) instance))))
	 (create-docker-image ()
	   (with-slots (steward dockerfile context cache build-time-variables) instance
	     (run-docker-engine-command
	      steward
	      "build" "--progress=plain"
	      (unless cache
		(list "--no-cache"))
	      (when build-time-variables
		(loop :for (name . value) :in build-time-variables
		      :append (list "--build-arg" (concatenate 'string name "=" value))))
	      "--file" (namestring dockerfile)
	      "--tag" (image-name instance)
	      (namestring context))))
	 (update-identifier-and-state ()
	   (with-slots (steward identifier state id) instance
	     (let ((properties
		     (probe-docker-image steward (image-name instance))))
	       (setf id (getf properties :id)
		     identifier (getf properties :id)
		     state t)))))
    (return-early-when-image-already-exists instance)
    (create-docker-image)
    (update-identifier-and-state)
    (update-instance-from-resource instance)))

(defmethod delete-resource ((instance docker-image))
  (flet ((ensure-that-resource-still-exists ()
	   (with-slots (steward identifier) instance
	     (unless (probe-docker-image steward identifier)
	       (resource-no-longer-exists
		'delete-resource instance
		"Docker image no longer exists."))))
	 (delete-image ()
	   (with-slots (steward identifier) instance
	     (let ((deleted-image
		     (run-docker-engine-command steward "image" "rm" (image-name instance))))
	       (flet ((compare-event (slot-reader regex)
			(ppcre:register-groups-bind (value) (regex deleted-image)
			  (unless (string= value (funcall slot-reader instance))
			    (resource-error 'delete-resource instance
					    "Docker image cannot be deleted."
				 "Cannot delete docker image ~A" (image-name instance))))))
		 (compare-event #'image-name "Untagged: (.*)")
		 (compare-event #'image-id "Deleted: (.*)")))))
	 (update-state-and-identifier ()
	   (with-slots (identifier state id) instance
	     (setf state nil
		   identifier nil
		   id nil))))
    (ensure-that-resource-still-exists)
    (delete-image)
    (update-state-and-identifier)))

#| 
(defun reclaim-images ()
  "Reclaim docker images which are no longer in use."
  (flet ((has-no-name-p (image)
	   (not (image-name image)))
	 (testsuite-p (image)
	   (uiop:string-prefix-p "testsuite" (image-tag image))))
    (mapcar #'delete-image
	    (loop :for image :in (list-images)
		  :when (or
			 (testsuite-p image)
			 (has-no-name-p image))
		  :collect image))))
|#


#|
(defun find-image (designator)
  "Find image designated by DESIGNATOR."
  (typecase designator
    (image     
     designator)
    (string
     (or
      (find designator (list-images)
	    :key #'image-name
	    :test #'string=)
      (when (= 12 (length designator))
	(find designator (list-images)
	      :key #'image-short-id
	      :test #'string=))
      (when (position #\: designator)
	(find designator (list-images)
	      :key #'image-id
	      :test #'string=))
      (find (concatenate 'string "sha256:" designator) (list-images)
	    :key #'image-id
	    :test #'string=)))))
|#

;;;; End of file `docker-engine.lisp'
