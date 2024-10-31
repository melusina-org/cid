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
   "The class represents a steward creating resources in a Keycloak installation.")
  (:documentation
   "The class represents a steward creating resources in a Keycloak installation."))

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
    :initarg :status-text)
   (method
    :initarg :method)))

(defparameter *trace-keycloak-admin-request* nil)

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
		      (when *trace-keycloak-admin-request*
			(format *trace-keycloak-admin-request*
				"Keycloak Admin Request: ~A ~A~&"
				method location)
			(write-string response *trace-keycloak-admin-request*))
		      (yason:parse response)))))
	    ((<= 400 status-code 599)
	     (error 'keycloak-admin-error
		    :status-text status-text
		    :uri uri
		    :headers headers
		    :status-code status-code
		    :body (flexi-streams:octets-to-string
			   body
			   :external-format :utf-8)
		    :method
		    (if content
			(list method content)
			method)))))))))


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
			state identifier parent external)
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
	       (resource-error
		'create-resource instance
		"Keycloak realm already exists."
		"~@<There is already an existing keycloak realm under the name ~A therefore an additional keycloak realm with the same name cannot be created.~:@>" realm))))
	 (create-keycloak-realm ()
	   (with-slots (steward realm) instance
	     (let ((content
		     (make-hash-table :test 'equal)))
	       (loop :for spec :in +keycloak-realm-model+
		     :for slot-name = (getf spec :slot-name)
		     :unless (eq :virtual (getf spec :kind))
		     :do (let ((name
				 (getf spec :json-name))
			       (value
				 (ecase (getf spec :type)
				   ((boolean string nullable-string)
				    (slot-value instance slot-name))
				   (authorization
				    (ecase (slot-value instance slot-name)
				      (:allow 'yason:true)
				      (:deny 'yason:false))))))
			   (setf (gethash name content) value)))
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



;;;;
;;;; Keycloak Client
;;;;

(alexandria:define-constant +keycloak-client-model+
    '((:slot-name parent
       :type keycloak-realm
       :initarg :parent
       :kind :virtual
       :documentation "The Keycloak realm of the client.")
      (:slot-name client
       :type string
       :initarg :client
       :json-name "clientId"
       :documentation
       "The client identifier registered with the identity provider.")
      (:slot-name identifier
       :type string
       :initarg :identifier
       :json-name "id"
       :documentation
       "The unique identifier for the client.")
      (:slot-name displayname
       :type nullable-string
       :initarg :displayname
       :initform nil
       :json-name "name"
       :documentation
       "Specifies display name of the client. For example 'My Client'.
Supports keys for localized values as well. For example: ${my_client}")
      (:slot-name description
       :type nullable-string
       :initarg :description
       :initform nil
       :json-name "description"
       :documentation
       "Specifies description of the client. For example 'My Client for TimeSheets'.
Supports keys for localized values as well. For example: ${my_client_description}")
      (:slot-name root-url
       :type nullable-string
       :initarg :root-url
       :initform nil
       :json-name "rootUrl"
       :documentation "Root URL appended to relative URLs.")
      (:slot-name admin-url
       :type nullable-string
       :initarg :admin-url
       :initform nil
       :json-name "adminUrl"
       :documentation "URL to the admin interface of the client.
Set this if the client supports the adapter REST API. This REST API allows
the auth server to push revocation policies and other administrative tasks.
Usually this is set to the base URL of the client.")
      (:slot-name home-url
       :type nullable-string
       :initarg :home-url
       :initform nil
       :json-name "baseUrl"
       :documentation "Default URL to use when the auth server needs to redirect or link back to the client.")
      (:slot-name enabled
       :type boolean
       :initarg :enabled
       :initform nil
       :json-name "enabled"
       :documentation "Disabled clients cannot initiate a login or have obtained access tokens.")
      (:slot-name public-client
       :type boolean
       :initarg :public-client
       :initform t
       :json-name "publicClient"
       :documentation "This defines the type of the OIDC client.
When it's NIL, the OIDC type is set to confidential access type. When it's T, it is set
to public access type.")
      (:slot-name always-display-in-console
       :type boolean
       :initarg :always-display-in-console
       :initform nil
       :json-name "alwaysDisplayInConsole"
       :documentation "Always list this client in the Account UI
even if the user does not have an active session.")
      (:slot-name client-authenticator-type
       :type string
       :initarg :client-authenticator-type
       :initform "client-secret"
       :json-name "clientAuthenticatorType"
       :documentation "Client Authenticator used for authentication of this client
against Keycloak server.")
      (:slot-name secret
       :type string
       :initarg :secret
       :initform nil
       :json-name "secret"
       :documentation "Client secret, when the OIDC type is set to confidential access."
       :confidential t)
      (:slot-name default-roles
       :type (list string)
       :initarg :default-roles
       :initform nil
       :json-name "defaultRoles"
       :kind :optional
       :documentation "Always list this client in the Account UI
even if the user does not have an active session.")
      (:slot-name redirect-uris
       :type (list string)
       :initarg :redirect-uris
       :initform nil
       :json-name "redirectUris"
       :documentation
       "Valid URI pattern a browser can redirect to after a successful login.
Simple wildcards are allowed such as 'http://example.com/*'. Relative path can
be specified too such as /my/relative/path/*. Relative paths are relative to
the client root URL, or if none is specified the auth server root URL is used.
For SAML, you must set valid URI patterns if you are relying on the consumer
service URL embedded with the login request.")
      (:slot-name web-origins
       :type (list string)
       :initarg :web-origins
       :initform nil
       :json-name "webOrigins"
       :documentation
       "Allowed CORS origins.
To permit all origins of Valid Redirect URIs, add '+'. This does not include
the '*' wildcard though. To permit all origins, explicitly add '*'.")
      (:slot-name bearer-only
       :type boolean
       :initarg :bearer-only
       :initform nil
       :json-name "bearerOnly")
      (:slot-name consent-required
       :type boolean
       :initarg :consent-required
       :initform nil
       :json-name "consentRequired")
      (:slot-name standard-flow-enabled
       :type boolean
       :initarg :standard-flow-enabled
       :initform t
       :json-name "standardFlowEnabled"
       :documentation
       "This enables standard OpenID Connect redirect based authentication with authorization code.
In terms of OpenID Connect or OAuth2 specifications, this enables support
of 'Authorization Code Flow' for this client.")
      (:slot-name implict-flow-enabled
       :type boolean
       :initarg :implict-flow-enabled
       :initform nil
       :json-name "implicitFlowEnabled"
       :documentation
       "This enables support for OpenID Connect redirect based authentication without authorization code.
In terms of OpenID Connect or OAuth2 specifications, this enables support
of 'Implicit Flow' for this client.")
      (:slot-name direct-access-grants-enabled
       :type boolean
       :initarg :direct-access-grants-enabled
       :initform t
       :json-name "directAccessGrantsEnabled"
       :documentation "This enables support for Direct Access Grants,
which means that client has access to username/password of user and exchange it directly
with Keycloak server for access token. In terms of OAuth2 specification, this enables
support of 'Resource Owner Password Credentials Grant' for this client.")
      (:slot-name service-accounts-enabled
       :type boolean
       :initarg :service-accounts-enabled
       :initform nil
       :json-name "serviceAccountsEnabled"
       :documentation "Allows you to authenticate this client to Keycloak and retrieve access token dedicated to this client.
In terms of OAuth2 specification, this enables support of 'Client Credentials Grant' for this client.")
      (:slot-name protocol
       :type string
       :initarg :protocol
       :initform "openid-connect"
       :json-name "protocol")
      (:slot-name default-client-scopes
       :type (list string)
       :initarg :default-client-scopes
       :initform '("web-origins" "acr" "roles" "profile" "basic" "email")
       :json-name "defaultClientScopes")
      (:slot-name optional-client-scopes
       :type (list string)
       :initarg :optional-client-scopes
       :initform '("address" "phone" "offline_access" "microprofile-jwt")
       :json-name "optionalClientScopes"))
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
			 (loop :for spec :in +keycloak-client-model+
			       :unless (member
					(getf spec :slot-name)
					'(displayname description parent))
			       :collect (slot-definition spec)))))
	       `(defclass keycloak-client (resource) ,(slot-definitions))))
	   (define-constructor ()
	     (let ((constructor-keys
		     (append
		      '(name displayname description 
			state identifier parent external)
		      (loop :for spec :in +keycloak-client-model+
			    :for slot-name = (getf spec :slot-name)
			    :unless (member slot-name '(identifier displayname description parent))
			    :collect slot-name))))
	       `(defun make-keycloak-client (&rest initargs &key keycloak-admin ,@constructor-keys)
		  "Make a keycloak client."
		  (declare (ignore ,@constructor-keys))
		  (check-type keycloak-admin keycloak-admin)
		  (apply #'make-instance 'keycloak-client
			 :steward keycloak-admin
			 (remove-property initargs :keycloak-admin))))))
  (progn
    (define-class)
    (define-constructor)))

(defmethod persistent-constructor ((class (eql 'keycloak-client)))
  'make-keycloak-client)

(macrolet ((define-persistent-slots ()
	     (labels ((persistence-definition (spec)
			(list
			 :slot-name (getf spec :slot-name)
			 :initarg (getf spec :initarg)
			 :confidential (getf spec :confidential)))
		      (persistence-definitions ()
			(cons
			 (list :slot-name 'steward
			       :initarg :keycloak-admin)
			 (loop :for spec :in +keycloak-client-model+
			       :collect (persistence-definition spec)))))
	       `(defmethod persistent-slots append ((instance keycloak-client))
		  (quote ,(persistence-definitions))))))
  (define-persistent-slots))

(macrolet ((define-examine-resource ()
	     (let ((slot-names
		     (loop :for spec :in +keycloak-client-model+
			   :collect (getf spec :slot-name)))
		   (properties
		     (loop :for spec :in +keycloak-client-model+
			   :collect (getf spec :initarg)
			   :collect (getf spec :slot-name))))
	       `(defmethod examine-resource append ((instance keycloak-client))
		  (with-slots ,slot-names instance
		    (list ,@properties))))))
  (define-examine-resource))

(defun probe-keycloak-client (steward client &key parent)
  (check-type client string)
  (flet ((extract-json-fields (object)
	   (extract-json-fields object +keycloak-client-model+))
	 (get-client ()
	   (keycloak-admin-request
	    steward
	    (concatenate 'string
			 "/admin/realms/"
			 (name parent)
			 "/clients/"
			 client)
	    :method :get)))
    (handler-case (extract-json-fields (get-client))
      (error (condition)
	(declare (ignore condition))
	(values nil)))))

(defmethod update-instance-from-resource ((instance keycloak-client))
  (flet ((update-instance (properties)
	   (unless properties
	     (resource-no-longer-exists
	      'update-instance-from-resource instance
	      "Keycloak Client no longer exists."))
	   (loop :for spec :in +keycloak-client-model+
		 :for indicator = (getf spec :initarg)
		 :always (getf properties indicator))
	   (loop :for spec :in +keycloak-client-model+
		 :for slot-name = (getf spec :slot-name)
		 :for indicator = (getf spec :initarg)
		 :unless (eq :virtual (getf spec :kind))
		 :do (setf (slot-value instance slot-name)
			   (getf properties indicator)))
	   (setf (slot-value instance 'state) t)))
    (with-slots (steward parent identifier) instance
      (update-instance (probe-keycloak-client steward identifier :parent parent)))))

(defmethod create-resource ((instance keycloak-client))
  (flet ((return-early-when-client-already-exists (instance)
	   (with-slots (steward parent identifier) instance
	     (when (and identifier (probe-keycloak-client steward identifier :parent parent))
	       (resource-error 'create-resource instance
			       "Keycloak client already exists."
			       "There is already an existing keycloak client under the name ~S
therefore the keycloak client ~A with the same name cannot be created." parent instance))))
	 (create-keycloak-client ()
	   (with-slots (steward client parent identifier) instance
	     (let ((content
		     (make-hash-table :test 'equal))
		   (uuid
		     (string-downcase
		      (with-output-to-string (buffer)
			(print-object (uuid:make-v1-uuid) buffer)))))
	       (setf identifier uuid)
	       (loop :for spec :in +keycloak-client-model+
		     :for slot-name = (getf spec :slot-name)
		     :for slot-type = (getf spec :type)
		     :unless (eq :virtual (getf spec :kind))
		     :do (let ((name
				 (getf spec :json-name))
			       (value
				 (cond
				   ((member slot-type
					    '(boolean string nullable-string
					      (list string))
					    :test #'equal)
				    (slot-value instance slot-name))
				   ((eq slot-type 'authorization)
				    (ecase (slot-value instance slot-name)
				      (:allow 'yason:true)
				      (:deny 'yason:false)))
				   (t
				    (error "Cannot handle slot type ~A" slot-type)))))
			   (setf (gethash name content) value)))
	       (let ((endpoint
		       (concatenate 'string
				    "/admin/realms/"
				    (name parent)
				    "/clients")))
		 (keycloak-admin-request steward endpoint
					 :method :post
					 :content content)))))
	 (update-state ()
	   (with-slots (state client) instance
	     (setf state t))))
    (return-early-when-client-already-exists instance)
    (create-keycloak-client)
    (update-state)))

(defmethod delete-resource ((instance keycloak-client))
  (flet ((ensure-that-resource-still-exists ()
	   (with-slots (steward parent identifier) instance
	     (unless (and identifier (probe-keycloak-client steward identifier :parent parent))
	       (resource-no-longer-exists
		'delete-resource instance
		"Keycloak client no longer exists."))))
	 (delete-client ()
	   (with-slots (steward realm identifier) instance
	     (keycloak-admin-request
	      steward
	      (concatenate 'string
			   "/admin/realms/"
			   (name realm)
			   "/clients/" identifier)
	      :method :delete)))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (ensure-that-resource-still-exists)
    (delete-client)
    (update-state-and-identifier)))

(defmethod update-resource-from-instance ((instance keycloak-client))
  (declare (optimize debug))
  (flet ((update-keycloak-client ()
	   (with-slots (steward client parent identifier) instance
	     (let ((content
		     (make-hash-table :test 'equal))
		   (client-endpoint
		     (concatenate 'string
				  "/admin/realms/"
				  (name parent)
				  "/clients/" identifier)))
	       (loop :for spec :in +keycloak-client-model+
		     :for slot-name = (getf spec :slot-name)
		     :for slot-type = (getf spec :type)
		     :unless (eq :virtual (getf spec :kind))
		     :do (let ((name
				 (getf spec :json-name))
			       (value
				 (cond
				   ((member slot-type
					    '(boolean string nullable-string
					      (list string))
					    :test #'equal)
				    (slot-value instance slot-name))
				   ((eq slot-type 'authorization)
				    (ecase (slot-value instance slot-name)
				      (:allow 'yason:true)
				      (:deny 'yason:false)))
				   (t
				    (error "Cannot handle slot type ~A" slot-type)))))
			   (setf (gethash name content) value)))
	       (keycloak-admin-request steward
				       client-endpoint
				       :method :put
				       :content content)))))
    (update-keycloak-client)))

;;;; End of file `keycloak-admin.lisp'
