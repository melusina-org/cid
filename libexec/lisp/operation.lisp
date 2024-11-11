;;;; operation.lisp — Project Operation for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/operation
  (:use #:cl)
  (:local-nicknames
   (#:cid #:org.melusina.cid))
  (:export
   ;; Project
   #:*project*
   #:project
   #:project-name
   #:project-status
   #:project-created-p
   #:project-hostname
   #:project-http-port
   #:project-https-port
   #:project-ssh-port
   #:project-pathname
   #:project-resources
   #:project-configuration-file
   #:edit-project-configuration-file
   #:project-backup-directory
   #:make-project
   #:list-projects
   #:create-project
   #:start-project
   #:stop-project
   #:restart-project
   #:delete-project
   #:project-configuration
   #:configure-project
   #:dump-project
   #:restore-project
   #:save-project
   #:load-project
   #:list-git-repositories
   #:create-git-repository
   #:delete-git-repository
   #:list-trac-environments
   #:pbcopy-keycloak-admin-password
   #:project-resources
   #:project-stewards
   #:enable-service
   #:disable-service
   #:service-enabled-p
   #:service-disabled-p
   #:run-console-server
   #:project-docker-engine
   #:project-docker-context
   ))

(in-package #:org.melusina.cid/operation)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defparameter *docker-compose*
  (system-relative-pathname #p"docker/compose/cid.yml")
  "The DOCKER-COMPOSE file used to run El Cid in the laboratory.")

(defvar *project* nil
  "The current project to operate on.")

(defclass project ()
  ((name
    :initarg :name
    :initform (error "A NAME is required.")
    :reader project-name)
   (docker-compose
    :initform *docker-compose*
    :reader project-docker-compose)
   (pathname
    :initform nil
    :reader project-pathname
    :documentation "The PATHNAME to the configuration files for project.")
   (tag
    :initarg :tag
    :initform "latest"
    :reader project-tag)
   (status
    :initarg :status
    :initform nil
    :reader project-status)
   (hostname
    :initarg :hostname
    :initform "localhost"
    :reader project-hostname
    :documentation "The HOSTNAME to the deployment of the project.")
   (http-port
    :initarg :http-port
    :initform 80
    :reader project-http-port
    :documentation "The HTTP PORT to the deployment of the project.")
   (https-port
    :initarg :https-port
    :initform 443
    :reader project-https-port
    :documentation "The HTTPS PORT to the deployment of the project.")
   (ssh-port
    :initarg :ssh-port
    :initform 22
    :reader project-ssh-port
    :documentation "The SSH PORT to the deployment of the project.")
   (keycloak-admin-password
    :initarg :keycloak-admin-password
    :initform (ironclad:byte-array-to-hex-string (ironclad:random-data 64)))
   (resources
    :initarg :resources
    :initform nil
    :documentation
    "The resources associated to the project.")
   (enable
    :initarg :enable
    :initform '(:trac :gitserver :jenkins)
    :documentation
    "The services to enable for the project.")))

(defun project-backup-directory (&optional (project *project*))
  (merge-pathnames #p"backups/" (project-pathname project)))

(defun project-configuration-file (&optional (project *project*))
  (merge-pathnames #p"cid.conf" (project-pathname project)))

(defun edit-project-configuration-file (&optional (project *project*))
  (uiop:run-program
   (list "open" "-a" "Emacs"
	 (namestring (project-configuration-file project)))))

(defun project-configuration-key (designator)
  "The configuration key for DESIGNATOR.
This is a string which can be used as a configuration key in configuration
files used by git."
  (flet ((key-component (object)
	   (etypecase object
	     (string
	      object)
	     (symbol
	      (string-downcase object)))))
    (apply
     #'concatenate
     'string
     (loop :for designator-part
	   :on (alexandria:ensure-list designator)
	   :collect (key-component (first designator-part))
	   :unless (null (rest designator-part))
	   :collect "."))))

(defun project-configuration (designator &optional (project *project*))
  (multiple-value-bind (output error status)
      (uiop:run-program
       (list "git" "config"
	     "--file" (namestring (project-configuration-file project))
	     (project-configuration-key designator))
       :output :string
       :error-output t
       :ignore-error-status t)
    (declare (ignore error))
    (when (eq status 0)
      (string-right-trim '(#\Newline) output))))

(defun (setf project-configuration) (value designator &optional (project *project*))
  (check-type value (or null string))
  (if value
      (uiop:run-program
       (list "git" "config"
	     "--file" (namestring (project-configuration-file project))
	     (project-configuration-key designator)
	     value)
       :output t
       :error-output t)
      (uiop:run-program
       (list "git" "config"
	     "--file" (namestring (project-configuration-file project))
	     "--unset"
	     (project-configuration-key designator))
       :output t
       :error-output t))
  (values value))

(defun is-yes-p (text)
  (when text
    (string= text "yes")))

(defun is-no-p (text)
  (when text
    (string= text "no")))

(defun service-enabled-p (service &optional (project *project*))
  (and (member service (slot-value project 'enable)) t))

(defun service-disabled-p (service &optional (project *project*))
  (not (service-enabled-p service project)))

(defun enable-service (service &optional (project *project*))
  (pushnew service (slot-value project 'enable))
  (values t))

(defun disable-service (service &optional (project *project*))
  (setf (slot-value project 'enable)
	(delete service (slot-value project 'enable)))
  nil)

(defun project-volume-database (&optional (project *project*))
  "The specification for Docker volumes consumed by PROJECT."
  (flet ((volume-name (name)
	   (concatenate 'string "cid" "-" (project-name project) "-" name)))
    (append
     (when (service-enabled-p :trac project)
       (list
	(list
	 :name "ssl"
	 :displayname "SSL"
	 :description "This volume holds TLS/SSL cryptographic artefacts."
	 :mount-point #p"/etc/ssl/private"
	 :volume (volume-name "ssl"))
	(list
	 :name "trac"
	 :displayname "Trac"
	 :description "This volume holds Trac assets and database."
	 :mount-point #p"/var/trac"
	 :volume (volume-name "trac"))
	(list
	 :name "www"
	 :displayname "WWW"
	 :description "This volume holds Apache site definitions for Trac."
	 :mount-point #p"/var/www"
	 :volume (volume-name "www"))))
     (when (or (service-enabled-p :trac project)
	       (service-enabled-p :gitserver project))
       (list
	(list
	 :name "git"
	 :displayname "GIT"
	 :description "This volume holds GIT repositories."
	 :mount-point #p"/var/git"
	 :volume (volume-name "git"))))
     (when (service-enabled-p :jenkins project)
       (list
	(list
	 :name "jenkins"
	 :displayname "Jenkins"
	 :description "This volume holds Jenkins configuration."
	 :mount-point #p"/var/lib/jenkins"
	 :volume (volume-name "jenkins")))))))

(defun project-created-p (project)
  (slot-value project 'status))

(defun make-project (&rest initargs &key name status hostname http-port https-port
  ssh-port docker-compose keycloak-admin-password tag resources pathname)
  (declare (ignore name hostname http-port https-port ssh-port status docker-compose keycloak-admin-password tag resources pathname))
  (apply #'make-instance 'project initargs))

(defun make-project-resources (&optional (project *project*))
  (let* ((cid:*tenant*
	   (cid:make-tenant :name "melusina"
			    :displayname "Melusina"))
	 (cid:*project*
	   (cid:make-project :name "local"
			     :displayname "Local Laboratory"
			     :tenant cid:*tenant*))
	 (keycloak-admin
	   (with-slots (keycloak-admin-password hostname https-port) project
	     (cid:make-keycloak-admin
	      :name "keycloak-admin"
	      :displayname "Keycloak Admin"
	      :description "The interface to the Keycloak instance
  providing identitiy services to the deployment."
	      :username "Administrator"
	      :location (concatenate
			 'string
			 "https://"
			 hostname
			 ":"
			 (write-to-string https-port)
			 "/authorization")
	      :password keycloak-admin-password)))
	 (keycloak-renaissance-realm
	   (cid:make-keycloak-realm
	    :keycloak-admin keycloak-admin
	    :name "renaissance"
	    :displayname "Common Lisp Renaissance"
	    :description "The Keycloak realm for Common Lisp Renaissance"
	    :realm "renaissance"
	    :brute-force-protected t
	    :login-with-email :allow
	    :reset-password :allow
	    :edit-username :deny))
	 (keycloak-renaissance-trac
	   (with-slots (hostname) project
	     (cid:make-keycloak-client
	      :keycloak-admin keycloak-admin
	      :parent keycloak-renaissance-realm
	      :client "trac"
	      :name "Renaissance Trac"
	      :displayname "Renaissance Trac"
	      :description "The OpenID Connect client providing authorisation for the Apache OIDC Module in Renaissance Trac."
	      :home-url (concatenate
			 'string
			 "https://"
			 hostname
			 "/trac/renaissance")
	      :redirect-uris
	      (list (concatenate
			 'string
			 "https://"
			 hostname
			 "/trac/renaissance/*"))
	      :web-origins
	      (list (concatenate 'string "https://" hostname))
	      :public-client nil)))
	 (docker-engine
	   (cid:make-docker-engine
	    :name "docker-engine"
	    :displayname "Docker Engine"
	    :description "The interface to the docker engine running the deployment."
	    :context
	    (if (string= "testsuite" (subseq (project-name project) 0 9))
		"colima-laboratory"
		(format nil "colima-~A" (project-name project)))))
	 (docker-volumes
	   (flet ((make-docker-volume (&key name displayname description volume &allow-other-keys)
		    (cid:make-docker-volume
		     :docker-engine docker-engine
		     :name name
		     :displayname displayname
		     :description description
		     :volume volume)))
	     (loop :for spec :in (project-volume-database project)
		    :collect (apply #'make-docker-volume spec))))
	 )
    (list :realms (list keycloak-renaissance-realm)
	  :clients (list keycloak-renaissance-trac)
	  :docker-volumes docker-volumes)))

(defmethod initialize-instance :after ((instance project) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((initialize-pathname ()
	   (with-slots (name pathname) instance
	     (setf pathname
		   (cid:user-data-relative-pathname
		    (concatenate 'string name "/")))))
	 (initialize-slots-from-configuration-file ()
	   (macrolet ((initialize-slot-from-configuration-file (slot-name designator)
			`(unless (slot-value instance ,slot-name)
			   (setf (slot-value instance ,slot-name)
				 (project-configuration ,designator instance)))))
	     (initialize-slot-from-configuration-file 'hostname '(:project :hostname))))
	 (initialize-resources ()
	   (with-slots (resources) instance
	     (unless resources
	       (setf resources (make-project-resources instance))))))
    (initialize-pathname)
    (initialize-slots-from-configuration-file)
    (initialize-resources)))

(defun project-url (object)
  (with-slots (hostname https-port) object
    (format nil "https://~A~@[:~A~]"
	    hostname (unless (= https-port 443) https-port))))

(defmethod print-object ((instance project) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (name tag) instance
      (format stream ":NAME ~S :TAG ~S :URL ~S" name tag (project-url instance)))))

(defmethod cid:persistent-constructor ((class (eql 'project)))
  'make-project)

(defmethod cid:persistent-slots append ((instance project))
  '((:initarg :name
     :slot-name name)
    (:initarg :hostname
     :slot-name hostname)
    (:initarg :http-port
     :slot-name http-port)
    (:initarg :https-port
     :slot-name https-port)
    (:initarg :ssh-port
     :slot-name ssh-port)
    (:initarg :docker-compose
     :slot-name docker-compose)
    (:initarg :tag
     :slot-name tag)
    (:initarg :keycloak-admin-password
     :slot-name keycloak-admin-password
     :confidential t)
    (:initarg :resources
     :slot-name resources)))

(defun project-resources (&optional (project *project*))
  "Project resources sorted in dependency order."
  (sort
   (loop :for (key resource) :on (slot-value project 'resources) :by #'cddr
	 :when (listp resource)
	 :append resource
	 :unless (listp resource)
	 :collect resource)
   #'cid:resource-require-p)) 

(defun (setf project-resources) (new-value &optional (project *project*))
  (setf (slot-value project 'resources) new-value))

(defun project-stewards (&optional (project *project*))
  (delete-duplicates
   (mapcar #'cid:steward (project-resources project))))

(defun project-docker-engine (&optional (project *project*))
  (find 'cid:docker-engine (project-stewards project)
	:key #'type-of))

(defun project-docker-context (&optional (project *project*))
  (let ((docker-engine
	  (project-docker-engine project)))
    (when docker-engine
      (slot-value docker-engine 'cid::context))))

(defmacro with-environment (bindings &body body)
  (alexandria:with-gensyms (saved-environment)
    `(let ((,saved-environment
	     (loop :for (name . value) :in ,bindings
		   :collect (cons name (uiop:getenv name))
		   :do (setf (uiop:getenv name) value))))
       (unwind-protect (progn ,@body)
	 (loop :for (name . value) :in ,saved-environment
	       :when value
	       :do (setf (uiop:getenv name) value))))))

(defmacro with-project-environment (project &body body)
  (alexandria:once-only (project)
    `(with-environment
	 (list (cons "cid_hostname"
		     (project-hostname ,project))
	       (cons "cid_http_port"
		     (write-to-string (project-http-port ,project)))
	       (cons "cid_https_port"
		     (write-to-string (project-https-port ,project)))
	       (cons "cid_ssh_port"
		     (write-to-string (project-ssh-port ,project)))
	       (cons "cid_image_tag"
		     (project-tag ,project))
	       (cons "cid_project"
		     (project-name ,project))
	       (cons "cid_keycloak_password"
		     (slot-value ,project 'keycloak-admin-password)))
       ,@body)))

(defun start-project (&optional (project *project*))
  (with-project-environment project
    (uiop:run-program
     (list "docker" "--context" (project-docker-context project)
	   "compose"
	   "--project-name" (project-name project)
	   "--file" (namestring
		     (project-docker-compose project))
	   "up" "--detach")
     :output t
     :error-output t)))

(defun stop-project (&optional (project *project*))
  (with-project-environment project
    (uiop:run-program
     (list "docker" "--context" (project-docker-context project)
	   "compose"
	   "--project-name" (project-name project)
	   "--file" (namestring
		     (project-docker-compose project))
	   "down")
     :output t
     :error-output t)))

(defun restart-project (&optional (project *project*))
  (stop-project project)
  (start-project project))

(defun create-project (&key project name tag hostname http-port https-port ssh-port (docker-compose *docker-compose*))
  (unless (or name tag project)
    (setf project *project*))
  (if project
      (setf name (project-name project)
	    docker-compose (project-docker-compose project)
	    tag (project-tag project)
	    hostname (project-hostname project)
	    http-port (project-http-port project)
	    https-port (project-https-port project)
	    ssh-port (project-ssh-port project))
      (progn
	(unless name
	  (error "A project requires a NAME."))
	(setf project
	      (make-project
	       :name name
	       :docker-compose docker-compose
	       :tag tag
	       :hostname hostname
	       :http-port http-port
	       :https-port https-port
	       :ssh-port ssh-port))))
  (flet ((return-early-when-project-resources-already-exist ()
	   (with-slots (status) project
	     (when status
	       (return-from create-project project))))
	 (create-project-directories ()
	   (dolist (pathname (list (project-pathname project)
				   (project-backup-directory project)))
	     (ensure-directories-exist pathname)))
	 (create-project-configuration-file ()
	   (let ((project-configuration-file
		   (project-configuration-file project)))
	     (unless (probe-file project-configuration-file)
	       (uiop:copy-file
		(system-relative-pathname "example/cid.conf")
		project-configuration-file))
	     (setf (project-configuration "project.hostname" project)
		   (slot-value project 'hostname))))
	 (create-docker-resources ()
	   (let ((docker-engine
		   (find 'cid:docker-engine (project-stewards project)
			 :key #'type-of
			 :test #'eq))
		 (docker-resources
		   (flet ((docker-resource-p (resource)
			    (eq (type-of (cid:steward resource))
				'cid:docker-engine)))
		     (remove-if-not #'docker-resource-p
				    (project-resources project)))))
	     (cid:configure-steward docker-engine)
	     (dolist (resource docker-resources)
	       (cid:create-resource resource))))
	 (wait-for-services-to-be-ready ()
	   (sleep 60))
	 (create-keycloak-resources ()
	   (let ((keycloak-admin
		   (find 'cid:keycloak-admin (project-stewards project)
			 :key #'type-of
			 :test #'eq))
		 (keycloak-resources
		   (flet ((keycloak-resource-p (resource)
			    (eq (type-of (cid:steward resource))
				'cid:keycloak-admin)))
		     (remove-if-not #'keycloak-resource-p
				    (project-resources project)))))
	     (cid:configure-steward keycloak-admin)
	     (dolist (resource keycloak-resources)
	       (cid:create-resource resource))))
	 (update-project-status ()
	   (with-slots (status) project
	     (setf status t))))
    (return-early-when-project-resources-already-exist)
    (create-project-directories)
    (create-project-configuration-file)
    (create-docker-resources)
    (configure-project project)
    (start-project project)
    (wait-for-services-to-be-ready)
    (create-keycloak-resources)
    (update-project-status)
    (values project)))

(defun delete-project (&optional (project *project*))
  (flet ((delete-keycloak-resources ()
	   (let ((keycloak-admin
		   (find 'cid:keycloak-admin (project-stewards project)
			 :key #'type-of
			 :test #'eq))
		 (keycloak-resources
		   (flet ((keycloak-resource-p (resource)
			    (eq (type-of (cid:steward resource))
				'cid:keycloak-admin)))
		     (remove-if-not #'keycloak-resource-p
				    (project-resources project)))))
	     (cid:configure-steward keycloak-admin)
	     (dolist (resource keycloak-resources)
	       (cid:delete-resource resource))))
	 (delete-containers-and-internal-volumes ()
	   (with-project-environment project
	     (uiop:run-program
	      (list "docker" "--context" (project-docker-context project)
		    "compose"
		    "--project-name" (project-name project)
		    "--file" (namestring
		     (project-docker-compose project))
		    "rm" "--stop" "--volumes" "--force")
	      :output t
	      :error-output t)))
	 (delete-docker-resources ()
	   (let ((docker-engine
		   (find 'cid:docker-engine (project-stewards project)
			 :key #'type-of
			 :test #'eq))
		 (docker-resources
		   (flet ((docker-resource-p (resource)
			    (eq (type-of (cid:steward resource))
				'cid:docker-engine)))
		     (remove-if-not #'docker-resource-p
				    (project-resources project)))))
	     (cid:configure-steward docker-engine)
	     (dolist (resource docker-resources)
	       (cid:delete-resource resource))))
	 (delete-docker-networks ()
	   (let ((docker-engine
		   (find 'cid:docker-engine (project-stewards project)
			 :key #'type-of
			 :test #'eq))
		 (docker-networks
		   (loop :for network :in '("frontend" "service")
			 :collect (concatenate
				   'string
				   (project-name project)
				   "_"
				   network))))
	     (apply #'cid::run-docker-engine-command
		    docker-engine "network" "rm" docker-networks)))
	 (delete-project-configuration-file ()
	   (uiop:delete-file-if-exists
	    (project-configuration-file project)))
	 (update-project-status ()
	   (with-slots (status) project
	     (setf status nil))))
    (delete-keycloak-resources)
    (delete-containers-and-internal-volumes)
    (delete-docker-resources)
    (delete-docker-networks)
    (delete-project-configuration-file)
    (update-project-status)
    (values project)))

(defun project-filename (&optional (designator *project*))
  (let ((project-name
	  (etypecase designator
	    (string
	     designator)
	    (project
	     (slot-value designator 'name)))))
    (cid:user-data-relative-pathname
     project-name
     "project.lisp")))

(defun save-encryption-key ()
  (unless cid:*encryption-key*
    (error "No encryption key."))
  (uiop:run-program
   (list "/usr/bin/security" "add-generic-password" "-U"
	 "-T" ""
	 "-s" "org.melusina.cid"
	 "-a" (slot-value *project* 'name)
	 "-w" (ironclad:byte-array-to-hex-string cid:*encryption-key*))))

(defun load-encryption-key (&optional (designator *project*))
  (let ((project-name
	  (etypecase designator
	    (string
	     designator)
	    (project
	     (slot-value designator 'name)))))
    (flet ((find-encryption-key ()
	     (uiop:run-program
	      (list "/usr/bin/security" "find-generic-password"
		    "-s" "org.melusina.cid"
		    "-a" project-name
		    "-w")
	      :output '(:string :stripped t))))
      (setf cid:*encryption-key*
	    (ironclad:hex-string-to-byte-array (find-encryption-key))))))

(defun save-project ()
  "Save *PROJECT* under PATHNAME."
  (let ((filename
	  (project-filename *project*)))
    (ensure-directories-exist filename)
    (save-encryption-key)
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*print-readably* t)
            (*print-circle* t)
            (*package* (find-package :keyword)))
	(cid:write-persistent-object *project* stream)
	(terpri stream))
      (finish-output stream))
    (values *project* filename)))

(defun load-project (designator)
  (let ((filename
	  (project-filename designator)))
    (assert (probe-file filename) () 'file-does-not-exist)
    (load-encryption-key designator)
    (with-open-file (stream filename :direction :input)
      (setf *project* (cid:read-persistent-object stream)))
    (values *project* filename)))


;;;;
;;;; Run Program
;;;;

(defun run-console-program (command &key (project *project*) volumes publish (output t) environment name hostname)
  (flet ((docker-bind (source destination)
	   (list
	    "--mount"
	    (format nil "type=bind,src=~A,dst=~A"
		    (namestring source)
		    (namestring destination))))
	 (docker-volume (spec)
	   (let ((volume
		   (getf spec :volume))
		 (mount-point
		   (getf spec :mount-point)))
	     (list
	      "--mount"
	      (format nil "type=volume,src=~A,dst=~A"
		      volume
		      (namestring mount-point)))))
	 (docker-image ()
	   (list (concatenate 'string
			      "cid/console:"
			      (project-tag project))))
	 (docker-environment (name value)
	   (list "--env"
		 (concatenate 'string name "=" value))))
    (uiop:run-program
     (append
      (list "docker" "--context" (project-docker-context project)
	    "run" "-i" "--rm")
      (when hostname
	(list "--hostname" hostname))
      (when name
	(list "--name" name))
      (loop :for spec :in (project-volume-database project)
	    :append (docker-volume spec))
      (loop :for (name . value) :in environment
	    :append (docker-environment name value))
      (docker-bind
       (project-backup-directory project)
       "/opt/cid/var/backups")
      (docker-bind
       (project-pathname project)
       "/opt/cid/var/config")
      volumes
      (when publish
	(loop :for spec :in publish
	      :collect "--publish"
	      :collect spec))
      (docker-image)
      command)
     :output output :error-output t)))

(defun run-console-server (&key (project *project*))
  (run-console-program
   nil
   :project project
   :name (concatenate 'string (project-name project) "-console-server")
   :hostname (concatenate 'string "console." (project-hostname project))
   :publish '("127.0.0.1:14005:4005")
   :environment
   (when cid:*encryption-key*
     (list
      (cons "CID_ENCRYPTION_KEY"
	    (ironclad:byte-array-to-hex-string
	     cid:*encryption-key*))))))


;;;;
;;;; Configure a PROJECT
;;;;

(defun configure-project (&optional (project *project*))
  "Configure a PROJECT.
The configuration of a PROJECT interacts with the software
run by the project and creates logical resources as described
by the PROJECT."
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_configure")
   :project project))


;;;;
;;;; Dump and Restore a PROJECT
;;;;

(defun dump-project (&optional (project *project*))
  "Dump PROJECT data.
This is not to be confused with SAVE-PROJECT."
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_dump" "-p" (project-name project))
   :project project))

(defun restore-project (pathname &optional (project *project*))
  "Restore a PROJECT."
  (flet ((docker-bind (source destination)
	   (list
	    "--mount"
	    (format nil "type=bind,src=~A,dst=~A"
		    (namestring source)
		    (namestring destination))))
	 (dockername (pathname)
	   (make-pathname :name (pathname-name pathname)
			  :type (pathname-type pathname)
			  :directory (list :absolute "var" "backups"))))
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_restore" (namestring (dockername pathname)))
   :project project
   :volumes (docker-bind
	     (namestring (truename pathname))
	     (namestring (dockername pathname))))))


;;;;
;;;; Administration of git Repositories
;;;;

(defun list-git-repositories (trac-environment &optional (project *project*))
  "List git repositories."
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_repository" "-t" trac-environment "ls")
   :project project
   :output :lines))

(defun create-git-repository (trac-environment name &optional (project *project*))
  "Create a git repository."
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_repository" "-t" trac-environment "create" name)
   :project project))
  
(defun delete-git-repository (trac-environment name &optional (project *project*))
  "Delete a git repository."
  (run-console-program
   (list "/bin/sh" "/opt/cid/bin/cid_repository" "-t" trac-environment "rm" name)
   :project project))


;;;;
;;;; Convenience
;;;;

(defun pbcopy-keycloak-admin-password (&optional (project *project*))
  "Copy keycloak admin password in the pasteboard."
  (with-input-from-string (password (slot-value project 'keycloak-admin-password))
    (uiop:run-program
     (list "/usr/bin/pbcopy")
     :input password)))

;;;;
;;;; Administration of Trac Environments
;;;;

(defun list-trac-environments (&optional (project *project*))
  "List trac environments."
  (flet ((reserved-name-p (string)
	   (position string '("ssl" "git" "www" "sites") :test #'string=))
	 (list-trac-directory ()
	   (run-console-program
	    (list "/bin/ls" "/var/trac")
	    :project project
	    :output :lines)))
    (remove-if #'reserved-name-p (list-trac-directory))))

;;;; End of file `operation.lisp'
