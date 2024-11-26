;;;; entry-point.lisp — Entrypoint for El Cid Administration Console

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/console)

(defun idle-loop ()
  (loop (sleep 5)))

(defun configure-console ()
  "Configure the Console environment.
This includes the following tasks:
 - Write sudoers file.
"
  (flet ((write-sudoers-file ()
	   (with-output-to-file (sudoers #p"/etc/sudoers.d/console"
					 :owner "root"
					 :mode #o600)
	     (write-trac-sudoers-policy sudoers))))
    (write-sudoers-file)))

(defun load-project ()
  (let ((pathname
	  #p"/opt/cid/var/config/project.lisp"))
    (assert (probe-file pathname) () 'file-does-not-exist)
    (let* ((cid:*encryption-key*
	     (ironclad:hex-string-to-byte-array
	      (uiop:getenv "CID_ENCRYPTION_KEY")))
	   (project
	     (with-open-file (stream pathname :direction :input)
	       (cid:read-persistent-object stream))))
      (when project
	(values project pathname)))))

(defun make-apache-trac* (&optional project)
  (unless project
    (setf project (load-project)))
  (let* ((project-resources
	   (operation:project-resources project))
	 (keycloak-client
	   (find 'cid:keycloak-client project-resources
		 :key #'type-of))
	 (provider-metadata-url
	   "https://forge.melusina.local/authorization/realms/melusina/.well-known/openid-configuration")
	 (redirect-uri
	   "https://forge.melusina.local/trac/login-with-oidc")
	 (oidc-configuration
	   (with-slots
		 ((client-id cid::client)
		  (client-secret cid::secret)
		  (client-displayname cid::displayname))
	       keycloak-client
	     (make-oidc-configuration
	      :client-id client-id
	      :auth-name client-displayname
	      :provider-metadata-url provider-metadata-url
	      :client-secret client-secret
	      :redirect-uri redirect-uri
	      :ssl-validate-server nil))))
    (make-apache-trac
     :name "trac-apache"
     :displayname "Trac/Apache Configuration."
     :tenant (cid:tenant keycloak-client)
     :project (cid:project keycloak-client)
     :description "Trac/Apache Configuration"
     :oidc-configuration oidc-configuration)))

(defun configure-apache-trac (&optional apache-trac)
  (unless apache-trac
    (setf apache-trac (make-apache-trac*)))
  (let ((oidc-conf
	  (merge-pathnames #p"sites/00-oidc.conf" *trac-data-directory*)))
    (with-output-to-file (oidc oidc-conf :owner "www-data"
					 :group "www-data"
					 :mode #o600)
      (write-apache-oidc-configuration (slot-value apache-trac 'oidc-configuration) oidc))))


(defun make-trac-environment* (name &key apache-trac project)
  (unless project
    (setf project (load-project)))
  (unless apache-trac
    (setf apache-trac (make-apache-trac* project)))
  (let* ((project
	   (or project (load-project)))
	 (apache-trac
	   (or (setf apache-trac (make-apache-trac* project)))))
    (make-trac-environment
     :name name
     :displayname (format nil "~A Trac environment" (string-capitalize name))
     :description "A Trac environment."
     :apache-trac apache-trac
     :location (concatenate 'string "/trac/" name))))


(defun configure-apache-trac-environment (environment)
  (let ((environment
	  (cond ((stringp environment)
		 (make-trac-environment* environment))
		(t
		 environment))))
    (with-output-to-file (conf (trac-environment-site-pathname environment)
			       :owner "www-data"
			       :group "www-data"
			       :mode #o644)
      (write-string
       (trac-environment-apache-configuration-document environment)
       conf))))

(defun configure-entry-point ()
  (format t "Administration Console Configuration for El Cid.~%")
  (uiop:quit 0))
			     
(defun server-entry-point ()
  (format t "Administration Console Server for El Cid.~%")
  (start-swank)
  (idle-loop)
  (uiop:quit 0))

(defun entry-point ()
  (let ((subcommand
	  (first (uiop:command-line-arguments))))
    (cond
      ((or (eq nil subcommand)
	   (string= "listen" subcommand))
       (server-entry-point))
      ((string= "configure" subcommand)
       (configure-entry-point))
      (t
       (error "The word ~S does not designate a console subcommand." subcommand)))))

;;;; End of file `entry-point.lisp'
