;;;; oidc.lisp — OpenID Connector

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/console)

(defclass oidc-configuration ()
  ((client-id
    :initarg :client-id
    :documentation
    "Client ID.")
   (auth-name
    :initarg :auth-name
    :documentation
    "Authorization name.")
   (provider-metadata-url
    :initarg :provider-metadata-url
    :documentation
    "This is copied from Keycloak “Realm settings” -> “General” -> “Endpoints” -> “OpenID Endpoint Configuration”.")
   (crypto-passphrase
    :initarg :crypto-passphrase
    :initform
    (with-output-to-string (buffer)
      (print-object (uuid:make-v1-uuid) buffer))
    :documentation
    "Free style string generated by uuidgen.")
   (client-secret
    :initarg :client-secret
    :documentation
    "Copy it from client credentials.")
   (redirect-uri
    :initarg :redirect-uri
    :documentation
    "The value of REDIRECT-URI must not point to a real file.
This is a vanity URL that will be served by mod_auth_openidc. However, this URI must be in
  a protected area in order for apache to request the auth_openidc module first.")
   (ssl-validate-server
    :initarg :ssl-validate-server
    :initform t
    :documentation
    "Wether to validate the server certificate."))
  (:documentation
   "Configuration for the Apache OpenID Connect plugin."))

(defun make-oidc-configuration (&rest initargs &key client-id auth-name provider-metadata-url crypto-passphrase client-secret redirect-uri ssl-validate-server)
  "Make a OIDC-CONFIGURATION steward."
  (declare (ignore client-id auth-name provider-metadata-url crypto-passphrase client-secret
		   redirect-uri ssl-validate-server))
  (apply #'make-instance 'oidc-configuration initargs))

(defmethod cid:persistent-constructor ((class (eql 'oidc-configuration)))
  'make-oidc-configuration)

(defmethod cid:persistent-slots append ((instance oidc-configuration))
client-id provider-metadata-url crypto-passphrase client-secret redirect-uri ssl-validate-server
  '((:initarg :client-id
     :slot-name client-id)
    (:initarg :auth-name
     :slot-name auth-name)
    (:initarg :provider-metadata-url
     :slot-name provider-metadata-url) 
    (:initarg :crypto-passphrase
     :slot-name crypto-passphrase
     :confidential t) 
    (:initarg :client-secret
     :slot-name client-secret
     :confidential t)
    (:initarg :redirect-uri
     :slot-name redirect-uri) 
    (:initarg :ssl-validate-server
     :slot-name ssl-validate-server)))

(defun write-apache-oidc-configuration (oidc stream)
  (let ((oidc-parameters
	  '(("OIDCProviderMetadataURL"
	     provider-metadata-url)
	    ("OIDCCryptoPassphrase"
	     crypto-passphrase)
	    ("OIDCClientID"
	     client-id)
	    ("OIDCClientSecret"
	     client-secret)
	    ("OIDCProviderTokenEndpointAuth"
	     "client_secret_basic")
	    ("OIDCRedirectURI"
	     redirect-uri)
	    ("OIDCXForwardedHeaders"
	     "X-Forwarded-Proto")
	    ("OIDCScope"
	     "\"openid email profile\"")
	    ("OIDCRemoteUserClaim"
	     "preferred_username")
	    ("OIDCPassClaimsAs"
	     "environment")
	    ("OIDCSSLValidateServer"
	     ssl-validate-server))))
    (flet ((write-oidc-parameter (name value)
	     (format stream "~A ~A~%"
		     name
		     (cond ((stringp value)
			    value)
			   ((string= "OIDCSSLValidateServer" name)
			    (if (slot-value oidc value)
				"On" "Off"))
			   ((symbolp value)
			    (slot-value oidc value))
			   (t
			    (error "Cannot interpret OIDC value ~A" value))))))
      (loop :for (name value) :in oidc-parameters
	    :do (write-oidc-parameter name value))

      (with-slots (auth-name redirect-uri) oidc
	(let ((redirect-location
		(subseq redirect-uri
			(or (position #\Slash redirect-uri
				      :start (length "https://"))
			    (error "Cannot derive location from REDIRECT-URI.")))))
	  (format stream
		  "~&<Location \"~A\">
  AuthType openid-connect
  AuthName \"~A\"
  Require valid-user
</Location>~%" redirect-location auth-name))))))

;;;; End of file `oidc.lisp'