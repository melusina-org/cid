;;;; keycloak.lisp — Keycloak Steward for El Cid

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
;;;; Authorization (Allow and Deny)
;;;;

(deftype authorization ()
  "The type of authorization attributes.
The possible values are :ALLOW and :DENY."
  `(member :allow :deny))

(defun authorization (object)
  "Convert OBJECT to an AUTHORIZATION value.
Auhtorization values are left untouched, otherwise
NIL is mapped to :DENY and other values to :ALLOW."
  (cond
    ((member object '(:allow :deny))
     object)
    (object
     :allow)
    (t
     :deny)))


;;;;
;;;; KeyCloak Admin
;;;;

(defclass keycloak-admin (steward)
  ((location
    :type string 
    :initarg :location
    :documentation "The location of the KeyCloak server.
For instance in a laboratory, http://localhost:8080.")
   (username
    :type string
    :initarg :username
    :documentation "The admin user name.")
   (password
    :type string
    :initarg :password
    :documentation "The admin password.")
   (access-token
    :type nullable-string
    :initform nil)
   (refresh-token
    :type nullable-string
    :initform nil))
  (:default-initargs
   :name "keycloak-admin"
   :displayname "Keycloak Admin"
   :description
   "The class represents a provider creating resources in a Keycloak installation.")
  (:documentation
   "The class represents a provider creating resources in a Keycloak installation."))

(defun make-keycloak-admin (&rest initargs &key tenant project name displayname description 
						location username password)
  "Make a KEYCLOAK-ADMIN steward."
  (declare (ignore tenant project name displayname description
		   location username password))
  (apply #'make-instance 'keycloak-admin initargs))

(defmethod persistent-constructor ((class (eql 'keycloak-admin)))
  'make-keycloak-admin)

(defmethod persistent-slots append ((instance keycloak-admin))
  '((:initarg :location
     :slot-name location)
    (:initarg :username
     :slot-name username)
    (:initarg :password
     :slot-name password
     :confidential t)))

;;;;
;;;; Get Keycloak Admin Token
;;;;

(defun get-keycloak-admin-token (instance)
  "Get a keycloak admin token from instance."
  (check-type instance keycloak-admin)
  (with-slots (location username password access-token refresh-token) instance
    (flet ((token-uri ()
	     (concatenate
	      'string location
	      "/realms/master/protocol/openid-connect/token"))
	   (decode-response (body)
	     (yason:parse
	      (flexi-streams:octets-to-string
	       body
	       :external-format :utf-8)))
	   (validate-response (response)
	     (unless (and
		      (gethash "access_token" response nil)
		      (gethash "refresh_token" response nil))
	       (error "Invalid keycloak response."))))
      (multiple-value-bind
	    (body
             status-code
             headers
             uri
             http-stream
             must-close
             status-text)
	  (drakma:http-request
	   (token-uri)
	   :method :post
	   :redirect 5
	   :content-type "application/x-www-form-urlencoded"
	   :external-format-out :utf-8
	   :parameters
	   (list
	    (cons "client_id" "admin-cli")
	    (cons "username" username)
	    (cons "password" password)
	    (cons "grant_type" "password")))
	(declare (ignore headers uri http-stream must-close))
	(cond
	  ((and (>= status-code 200) (< status-code 300))
	   (let ((response
		   (decode-response body)))
	     (validate-response response)
	     (setf access-token (gethash "access_token" response)
		   refresh-token (gethash "refresh_token" response))
	     (values t)))
	  ((= status-code 401)
	   (error "Keycloak Admin not authorized."))
	  (t
	   (error "Keycloak Admin general error.~&~A" status-text)))))))


;;;;
;;;; Configure Steward
;;;;

(defmethod configure-steward ((instance keycloak-admin))
  (get-keycloak-admin-token instance))


;;;;
;;;; Authorized API Requests
;;;;

(define-condition keycloak-admin-error (error)
  ((body
    :initarg :body)
   (status-code
    :initarg :status-code)
   (headers
    :initarg :headers)
   (uri
    :initarg :uri)
   (status-text
    :initarg :status-text)))

(defun keycloak-admin-request (instance location &key (method :get) content)
  (declare (optimize debug safety))
  (with-slots (access-token (base-url location)) instance
    (let ((yason:*symbol-key-encoder*
	    #'yason:encode-symbol-as-lowercase)) 
      (let ((location
	      (concatenate 'string base-url location))
	    (content
	      (cond
		((eq nil content)
		 content)
		((stringp content)
		 content)
		((hash-table-p content)
		 (yason:with-output-to-string* ()
		   (yason:encode content)))
		((plist-p content)
		 (yason:with-output-to-string* ()
		   (yason:encode-plist content)))
		((alist-p content)
		 (yason:with-output-to-string* ()
		   (yason:encode-alist content))))))
	(multiple-value-bind
	      (body
               status-code
               headers
               uri
               http-stream
               must-close
               status-text)
	    (drakma:http-request
	     location
	     :method method
	     :redirect 5
	     :content-type (when content "application/json")
	     :external-format-out (when content :utf-8)
	     :content content
	     :additional-headers
	     (list (cons "authorization"
			 (concatenate 'string "Bearer " access-token))))
	  (declare (ignore http-stream must-close))
	  (cond
	    ((<= 200 status-code 299)
	     (let ((response
		     (flexi-streams:octets-to-string
		      body
		      :external-format :utf-8)))
	       (cond ((string= response "")
		      nil)
		     (t
		      (yason:parse response)))))
	    ((<= 400 status-code 599)
	     (error 'keycloak-admin-error
		    :status-text status-text
		    :uri uri
		    :headers headers
		    :status-code status-code
		    :body (flexi-streams:octets-to-string
			   body
			   :external-format :utf-8)))))))))
	  

;;;;
;;;; Keycloak Realm
;;;;

(alexandria:define-constant +keycloak-realm-model+
    '((:slot-name realm
       :type string
       :initarg :realm
       :json-name "realm")
      (:slot-name displayname
       :type nullable-string
       :initarg :displayname
       :initform nil
       :json-name "displayName")
      (:slot-name displayname-html
       :type nullable-string
       :initarg :displayname-html
       :initform nil
       :json-name "displayNameHtml")
      (:slot-name enabled
       :type boolean
       :initarg :enabled
       :initform t
       :json-name "enabled")
      (:slot-name brute-force-protected
       :type boolean
       :initarg :brute-force-protected
       :initform t
       :json-name "bruteForceProtected")
      (:slot-name registration
       :type authorization
       :initarg :registration
       :initform :deny
       :documentation "Wether registration is allowed or denied.
Possibles values are :ALLOW and :DENY."
       :json-name "registrationAllowed")
      (:slot-name login-with-email
       :type authorization
       :initarg :login-with-email
       :initform :deny
       :documentation "Wether login with email is allowed or denied.
Possibles values are :ALLOW and :DENY."
       :json-name "loginWithEmailAllowed")
      (:slot-name duplicate-emails
       :type authorization
       :initarg :duplicate-emails
       :initform :deny
       :documentation "Wether duplicate emails are :ALLOW or :DENY."
       :json-name "duplicateEmailsAllowed")
      (:slot-name reset-password
       :type authorization
       :initarg :reset-password
       :initform :deny
       :documentation "Wether reset password is allowed or denied.
Possibles values are :ALLOW and :DENY."
       :json-name "resetPasswordAllowed")
      (:slot-name edit-username
       :type authorization
       :initarg :edit-username
       :initform :deny
       :documentation "Wether edit username is allowed or denied.
Possibles values are :ALLOW and :DENY."
       :json-name "editUsernameAllowed"))
  :test 'equal)

(macrolet ((define-class ()
	     (labels ((slot-definition (spec)
			(append
			 (list
			  (getf spec :slot-name)
			  :initarg (getf spec :initarg)
			  :reader (getf spec :slot-name))
			 (unless (eq :not-found
				     (getf spec :initform :not-found))
			   (list
			    :initform (getf spec :initform)))
			 (list
			  :documentation (getf spec :documentation))))
		      (slot-definitions ()
			(cons
			 '(steward-class
			   :type symbol
			   :initform 'keycloak-admin
			   :allocation :class)
			 (loop :for spec :in +keycloak-realm-model+
			       :unless (eq 'displayname (getf spec :slot-name))
			       :collect (slot-definition spec)))))
	       `(defclass keycloak-realm (resource) ,(slot-definitions))))
	   (define-constructor ()
	     (let ((constructor-keys
		     (append
		      '(name displayname description 
			state identifier external)
		      (loop :for spec :in +keycloak-realm-model+
			    :for slot-name = (getf spec :slot-name)
			    :unless (member slot-name '(displayname))
			    :collect slot-name))))
	       `(defun make-keycloak-realm (&rest initargs &key keycloak-admin ,@constructor-keys)
		  "Make a keycloak realm."
		  (declare (ignore ,@constructor-keys))
		  (check-type keycloak-admin keycloak-admin)
		  (apply #'make-instance 'keycloak-realm
			 :steward keycloak-admin
			 (remove-property initargs :keycloak-admin))))))
  (progn
    (define-class)
    (define-constructor)))

(defmethod persistent-constructor ((class (eql 'keycloak-realm)))
  'make-keycloak-realm)

(macrolet ((define-persistent-slots ()
	     (labels ((persistence-definition (spec)
			(list
			 :slot-name (getf spec :slot-name)
			 :initarg (getf spec :initarg)))
		      (persistence-definitions ()
			(cons
			 (list :slot-name 'steward
			       :initarg :keycloak-admin)
			 (loop :for spec :in +keycloak-realm-model+
			       :collect (persistence-definition spec)))))
	       `(defmethod persistent-slots append ((instance keycloak-realm))
		  (quote ,(persistence-definitions))))))
  (define-persistent-slots))

(macrolet ((define-examine-resource ()
	     (let ((slot-names
		     (loop :for spec :in +keycloak-realm-model+
			   :collect (getf spec :slot-name)))
		   (properties
		     (loop :for spec :in +keycloak-realm-model+
			   :collect (getf spec :initarg)
			   :collect (getf spec :slot-name))))
	       `(defmethod examine-resource append ((instance keycloak-realm))
		  (with-slots ,slot-names instance
		    (list ,@properties))))))
  (define-examine-resource))

(defun probe-keycloak-realm (steward realm)
  (flet ((extract-json-fields (object)
	   (extract-json-fields object +keycloak-realm-model+))
	 (get-realm ()
	   (keycloak-admin-request
	    steward
	    (concatenate 'string "/admin/realms/" realm))))
    (handler-case (extract-json-fields (get-realm))
      (error (condition)
	(declare (ignore condition))
	(values nil)))))

(defmethod update-instance-from-resource ((instance keycloak-realm))
  (flet ((update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Keycloak Realm no longer exists."))
	   (loop :for spec :in +keycloak-realm-model+
		 :for indicator = (getf spec :initarg)
		 :always (getf properties indicator))
	   (loop :for spec :in +keycloak-realm-model+
		 :for slot-name = (getf spec :slot-name)
		 :for indicator = (getf spec :initarg)
		 :do (setf (slot-value instance slot-name)
			   (getf properties indicator)))
	   (setf (slot-value instance 'state) t)))
    (with-slots (steward identifier) instance
      (update-instance (probe-keycloak-realm steward identifier)))))

(defmethod create-resource ((instance keycloak-realm))
  (flet ((return-early-when-realm-already-exists (instance)
	   (with-slots (steward realm) instance
	     (when (probe-keycloak-realm steward realm)
	       (resource-error 'create-resource instance
			       "Keycloak realm already exists."
			       "There is already an existing keycloak realm under the name ~S
therefore the keycloak realm ~A with the same name cannot be created." realm instance))))
	 (create-keycloak-realm ()
	   (with-slots (steward realm) instance
	     (let ((content
		     (make-hash-table :test 'equal)))
	       (loop :for spec :in +keycloak-realm-model+
		     :for slot-name = (getf spec :slot-name)
		     :for name = (getf spec :json-name)
		     :for value = (ecase (getf spec :type)
				    ((boolean string nullable-string)
				     (slot-value instance slot-name))
				    (authorization
				     (ecase (slot-value instance slot-name)
				       (:allow 'yason:true)
				       (:deny 'yason:false))))
		     :do (setf (gethash name content) value))
	       (keycloak-admin-request steward "/admin/realms"
				       :method :post
				       :content content))))
	 (update-identifier-and-state ()
	   (with-slots (identifier state realm) instance
	     (setf identifier realm
		   state t))))
    (return-early-when-realm-already-exists instance)
    (create-keycloak-realm)
    (update-identifier-and-state)))

(defmethod delete-resource ((instance keycloak-realm))
  (flet ((ensure-that-resource-still-exists ()
	   (with-slots (steward realm) instance
	     (unless (probe-keycloak-realm steward realm)
	       (resource-no-longer-exists
		'delete-resource instance
		"Keycloak realm no longer exists."))))
	 (delete-realm ()
	   (with-slots (steward realm) instance
	     (keycloak-admin-request
	      steward
	      (concatenate 'string "/admin/realms/" realm)
	      :method :delete)))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (ensure-that-resource-still-exists)
    (delete-realm)
    (update-state-and-identifier)))

(defmethod update-resource-from-instance ((instance keycloak-realm))
  (flet ((update-keycloak-realm ()
	   (with-slots (steward realm) instance
	     (let ((content
		     (make-hash-table :test 'equal))
		   (realm-endpoint
		     (concatenate 'string "/admin/realms/" realm)))
	       (loop :for spec :in +keycloak-realm-model+
		     :for slot-name = (getf spec :slot-name)
		     :for name = (getf spec :json-name)
		     :for value = (ecase (getf spec :type)
				    ((boolean string nullable-string)
				     (slot-value instance slot-name))
				    (authorization
				     (ecase (slot-value instance slot-name)
				       (:allow t)
				       (:deny nil))))
		     :do (setf (gethash name content) value))
	       (keycloak-admin-request steward
				       realm-endpoint
				       :method :put
				       :content content)))))
    (update-keycloak-realm)))


#|

(defmethod update-resource-from-instance ((instance docker-volume))
  nil)


;;;;
;;;; Docker Compose Project
;;;;

(defclass docker-project (resource)
  ((steward-class
    :type symbol
    :initform 'keycloak-admin
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

(defun make-docker-project (&rest initargs &key keycloak-admin name displayname description
						state identifier external
						project pathname volumes environment)
  "Make a docker compose project."
  (declare (ignore name displayname description
		   state identifier external
		   project pathname volumes environment))
  (check-type keycloak-admin keycloak-admin)
  (apply #'make-instance 'docker-project
	 :steward keycloak-admin
	 (remove-property initargs :keycloak-admin)))

(defmethod persistent-constructor ((class (eql 'docker-project)))
  'make-docker-project)

(defmethod persistent-slots append ((instance docker-project))
  '((:initarg :keycloak-admin
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
   (run-keycloak-admin-command steward "compose" "ls" "--format" "json")
   `((:property :project
      :name "Name"
      :type string)
     (:property :state
      :name "Status"
      :type string
      :key ,#'docker-project-status)
     (:property :pathname
      :name "ConfigFiles"
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

(defmethod list-resource-identifiers ((steward keycloak-admin) (resource-class (eql 'docker-project)))
  (flet ((project-name (plist)
	   (getf plist :project)))
    (mapcar #'project-name (probe-docker-projects steward))))

(defmethod list-resources ((steward keycloak-admin) (resource-class (eql 'docker-project)))
  (loop :for properties :in (probe-docker-projects steward)
	:collect (apply #'make-docker-project
			:keycloak-admin steward
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
		   (run-keycloak-admin-command steward "compose"
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
		   (run-keycloak-admin-command steward "compose"
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

|#

;;;; End of file `keycloak-admin.lisp'
