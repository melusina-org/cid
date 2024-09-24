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
   (#:docker #:org.melusina.cid/docker)
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
   #:project-configuration-file
   #:edit-project-configuration-file
   #:project-backup-directory
   #:make-project
   #:list-projects
   #:create-project
   #:update-project
   #:start-project
   #:stop-project
   #:restart-project
   #:find-project
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
    :reader project-resources
    :documentation
    "The resources associated to the project.")
   (volumes
    :initform nil)))

(defun project-backup-directory (&optional (project *project*))
  (merge-pathnames #p"backups/" (project-pathname project)))

(defun project-configuration-file (&optional (project *project*))
  (merge-pathnames #p"cid.conf" (project-pathname project)))

(defun edit-project-configuration-file (&optional (project *project*))
  (uiop:run-program
   (list "open" "-a" "EmacsMac"
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
  (is-yes-p
   (project-configuration (list service :service :enable) project)))

(defun service-disabled-p (service &optional (project *project*))
  (is-no-p
   (project-configuration (list service :service :enable) project)))

(defun enable-service (service &optional (project *project*))
  (setf
   (project-configuration (list service :service :enable) project)
   "yes")
  t)

(defun disable-service (service &optional (project *project*))
  (setf
   (project-configuration (list service :service :enable) project)
   "no")
  nil)

(defun make-volume (name project)
  (docker:make-volume
   :name (concatenate 'string "cid" "-" (project-name project) "-" name)))

(defun volume-database (project)
  (flet ((volume-name (binding)
	   (docker:volume-name (car binding))))
    (remove-duplicates
     (append
      (when (service-enabled-p :trac project)
	(list
	 (cons (make-volume "ssl" project) #p"/etc/ssl/private")
	 (cons (make-volume "trac" project) #p"/var/trac")
	 (cons (make-volume "www" project) #p"/var/www")
	 (cons (make-volume "git" project) #p"/var/git")))
      (when (service-enabled-p :gitserver project)
	(list
	 (cons (make-volume "git" project) #p"/var/git")))
      (when (service-enabled-p :jenkins project)
	(list
	 (cons (make-volume "jenkins" project) #p"/var/lib/jenkins"))))
     :test #'string=
     :key #'volume-name)))

(defun project-created-p (project)
  (slot-value project 'status))

(defmethod initialize-instance :after ((instance project) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (macrolet ((initialize-slot-from-configuration-file (slot-name designator)
	       `(unless (slot-value instance ,slot-name)
		  (setf (slot-value instance ,slot-name)
			(project-configuration ,designator instance)))))
    (with-slots (name pathname volumes http-port https-port ssh-port) instance
      (setf volumes
 	    (loop :for system :in '("ssl" "trac" "git" "www" "jenkins")
		  :collect (docker:make-volume
			    :name (concatenate 'string "cid-" name "-" system))))
      (setf pathname
	    (cid:user-data-relative-pathname (concatenate 'string name "/")))
      (initialize-slot-from-configuration-file 'hostname '(:project :hostname)))))

(defun make-project (&rest initargs &key name status hostname http-port https-port
  ssh-port docker-compose keycloak-admin-password tag resources)
  (declare (ignore name hostname http-port https-port ssh-port status docker-compose keycloak-admin-password tag resources))
  (apply #'make-instance 'project initargs))

(defun make-project-resources (project)
  (let* ((cid:*tenant*
	   (cid:make-tenant :name "melusina"
			    :displayname "Melusina"))
	 (cid:*project*
	   (cid:make-project :name "local"
			     :displayname "Local Laboratory"
			     :tenant cid:*tenant*))
	 (keycloak-admin
	   (with-slots (keycloak-admin-password hostname) project
	     (cid:make-keycloak-admin :name "keycloak-admin"
				      :displayname "Keycloak Admin"
				      :description "The interface to the Keycloak instance
  providing identitiy services to the deployment."
				      :username "Administrator"
				      :location (concatenate
						 'string
						 "https://"
						 hostname
						 "/authorization")
				      :password keycloak-admin-password)))
	 (keycloak-renaissance-realm
	   (cid:make-keycloak-realm :keycloak-admin keycloak-admin
				    :name "renaissance"
				    :displayname "Common Lisp Renaissance"
				    :description "The Keycloak realm for Common Lisp Renaissance"
				    :realm "renaissance"
				    :brute-force-protected t
				    :login-with-email :allow
				    :reset-password :allow
				    :edit-username :deny)))
    (list :realms (list keycloak-renaissance-realm))))

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

(defun list-projects ()
  (flet ((project-name (volume-name)
	   (multiple-value-bind (match-start match-end reg-starts reg-ends)
	       (ppcre:scan "cid-([^-]+)-git" volume-name)
	     (declare (ignore match-end))
	     (when match-start
	       (subseq volume-name (aref reg-starts 0) (aref reg-ends 0))))))
    (loop :for volume :in (docker:list-volumes)
	  :for project-name = (project-name (docker:volume-name volume))
	  :when project-name
	  :collect (make-project :name project-name :status t))))

(defun find-project (designator)
  "Find project designated by DESIGNATOR."
  (typecase designator
    (project     
     designator)
    (string
     (find designator (list-projects)
	   :key #'project-name
	   :test #'string=))))

(defun update-project (&optional (project *project*))
  "Update PROJECT slots from its actual state."
  (let ((actual-state
	  (find-project (project-name project))))
    (unless actual-state
      (setf (slot-value project 'status) nil)
      (return-from update-project project))
    (loop :for slot-name :in '(status volumes)
	  :do (setf (slot-value project slot-name)
		    (slot-value actual-state slot-name)))
    (loop :for volume :in (slot-value project 'volumes)
	  :do (docker:update-volume volume))
    (values project)))

(defun project-resources (project)
  (loop :for (key resource) :on (slot-value project 'resources)
	:when (listp resource)
	:append resource
	:unless (listp resource)
	:collect resource))

(defun project-stewards (project)
  (delete-duplicates
   (mapcar #'cid:steward (project-resources project))))

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
  (with-slots (resources volumes status) project
    (when status
      (return-from create-project project))
    (dolist (pathname (list (project-pathname project)
			    (project-backup-directory project)))
      (ensure-directories-exist pathname))
    (let ((project-configuration-file
	    (project-configuration-file project)))
      (unless (probe-file project-configuration-file)
	(uiop:copy-file
	 (system-relative-pathname "example/cid.conf")
	 project-configuration-file)))
    (loop :for volume :in volumes
	  :do (docker:create-volume
	       :name (docker:volume-name volume)
	       :driver (docker:volume-driver volume)))
    (dolist (steward (project-stewards project))
      (cid:configure-steward steward))
    (dolist (resource (project-resources project))
      (cid:create-resource resource))
    (setf status t)
    (values project)))

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
     (list "docker" "compose"
	   "--project-name" (project-name project)
	   "--file" (namestring
		     (project-docker-compose project))
	   "up" "--detach")
     :output t
     :error-output t)))

(defun stop-project (&optional (project *project*))
  (with-project-environment project
    (uiop:run-program
     (list "docker" "compose"
	   "--project-name" (project-name project)
	   "--file" (namestring
		     (project-docker-compose project))
	   "down")
     :output t
     :error-output t)))

(defun restart-project (&optional (project *project*))
  (stop-project project)
  (start-project project))

(defun delete-project (&optional (project *project*))
  (with-project-environment project
    (uiop:run-program
     (list "docker" "compose"
	   "--project-name" (project-name project)
	   "--file" (namestring
		     (project-docker-compose project))
	   "rm" "--volumes" "--force")
     :output t
     :error-output t)
    (with-slots (pathname) project
      (uiop:delete-file-if-exists pathname))
    (with-slots (volumes status) project
      (loop :for volume :in volumes
	    :do (docker:delete-volume volume))
      (setf status nil)))
  (values project))

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

(defun run-console-program (command &key (project *project*) volumes (output t))
  (flet ((docker-bind (source destination)
	   (list
	    "--mount"
	    (format nil "type=bind,src=~A,dst=~A"
		    (namestring source)
		    (namestring destination))))
	 (docker-volume (binding)
	   (list
	    "--mount"
	    (format nil "type=volume,src=~A,dst=~A"
		    (docker:volume-name (car binding))
		    (namestring (cdr binding)))))
	 (docker-image ()
	   (list (concatenate 'string
			      "cid/console:"
			      (project-tag project)))))
    (uiop:run-program
     (append
      (list "docker" "run" "-i" "--rm")
      (loop :for binding :in (volume-database project)
	    :append (docker-volume binding))
      (docker-bind
       (project-backup-directory project)
       "/opt/cid/var/backups")
      (docker-bind
       (project-pathname project)
       "/opt/cid/var/config")
      volumes
      (docker-image)
      command)
     :output output :error-output t)))


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
;;;; Administration of Trac Environments
;;;;

(defun list-trac-environments (&optional (project *project*))
  "List trac environments."
  (flet ((reserved-name-p (string)
	   (position string '("git" "www" "sites") :test #'string=))
	 (list-trac-directory ()
	   (run-console-program
	    (list "/bin/ls" "/var/trac")
	    :project project
	    :output :lines)))
    (remove-if #'reserved-name-p (list-trac-directory))))

;;;; End of file `operation.lisp'
