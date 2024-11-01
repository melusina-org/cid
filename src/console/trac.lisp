;;;; trac.lisp — Trac integration

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

(defparameter *trac-data-directory* #p"/var/trac/"
  "The directory holding Trac environment data.")

(defparameter *trac-www-directory* #p"/var/www/"
  "The directory holding webserver sites configuration.")

(defparameter *trac-git-directory* #p"/var/git/"
  "The directory holding git repositories.")

(defparameter *trac-admin-pathname* #p"/usr/bin/trac-admin"
  "Path to the trac-admin utility.")

(defparameter *trac-permissions-database*
  '(("admin" . ("TRAC_ADMIN"))
    ("developer" . ("WIKI_ADMIN" "REPORT_ADMIN" "TICKET_MODIFY"))))



;;;;
;;;; Apache Trac
;;;;

(defclass apache-trac (cid:steward)
  ((oidc-configuration
    :initarg :oidc-configuration))
  (:documentation
   "The class represents a steward creating and maintaining Trac environments served by an
  Apache server."))

(defun make-apache-trac (&rest initargs &key tenant project name displayname description oidc-configuration)
  "Make an APACHE-TRAC steward."
  (declare (ignore tenant project name displayname description oidc-configuration))
  (apply #'make-instance 'apache-trac initargs))

(defmethod cid:persistent-constructor ((class (eql 'apache-trac)))
  'make-apache-trac)

(defmethod cid:persistent-slots append ((instance apache-trac))
  '((:initarg :oidc-configuration
     :slot-name oidc-configuration)))

(defmethod cid:configure-steward ((instance apache-trac))
  (flet ((check-trac-installation ()
	   "Ensure we are running on a valid trac installation.
If we cannot validate that we are running on a valid trac installation,
then an error is signaled, otherwise NIL is returned."
  (let ((required-directories (list *trac-data-directory*
				    *trac-www-directory*
				    *trac-git-directory*)))
    (loop :for directory :in required-directories
	  :do (unless (probe-file directory)
	       (error "The directory ~a cannot be read.
This directory is required in a valid Trac installation but it could not
be read on this system." directory))))))
    (check-trac-installation)))

(defun write-trac-sudoers-policy (&optional (stream t))
  (let ((trac-environment-directory
	  (merge-pathnames #p"environment/" *trac-data-directory*)))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "www-data"
     :command
     (format nil "~A ~A*"
	     (namestring *trac-admin-pathname*)
	     (namestring trac-environment-directory)))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "www-data"
     :command
     (format nil "~A ~A -maxdepth 1 -mindepth 1"
	     "/usr/bin/find"
	     (namestring trac-environment-directory)))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "root"
     :command
     (format nil "/usr/bin/install -o cid -g www-data -m ??? /dev/null /var/trac/sites/*"))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "root"
     :command
     (format nil "/usr/bin/chown www-data /var/trac/sites/*"))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "root"
     :command
     (format nil "/bin/mv -f /var/trac/sites/* /var/trac/sites/*"))
    (write-sudoers-policy
     :stream stream
     :user "cid"
     :run-as "root"
     :command
     (format nil "/bin/rm -f /var/trac/sites/.*"))))

(defmethod cid:list-resource-identifiers ((steward apache-trac)
					   (resource-class (eql 'trac-environment)))
  "The list of Trac environments configured in this deployment."
  (declare (ignore steward resource-class))
  (flet ((environment-of-directory (directoryname)
	   (ppcre:regex-replace ".*/" directoryname ""))
	 (find-environments (trac-environment-directory)
	   (run-program
	    (list
	     (namestring #p"/usr/bin/find")
	     (namestring trac-environment-directory)
	     "-maxdepth" "1"
	     "-mindepth" "1")
	    :identity "www-data"
	    :output :lines))
	 (trac-environment-name (filenames)
	   (loop :for filename :in filenames
		 :collect (pathname-name (pathname filename)))))
    (let ((trac-environment-directory
	    (merge-pathnames #p"environment/" *trac-data-directory*)))
      (if (probe-file trac-environment-directory)
	  (trac-environment-name (find-environments trac-environment-directory))
	  nil))))


;;;;
;;;; Trac Environment
;;;;

(defclass trac-environment (cid:resource)
  ((cid:steward-class
    :type symbol
    :initform 'apache-trac
    :allocation :class)
   (location
    :initarg :location
    :initform nil
    :documentation "The WEB Location of environment.")
   (permissions
    :initarg :permissions
    :initform *trac-permissions-database*)))

(defun make-trac-environment (&rest initargs &key apache-trac name displayname description
						  state identifier parent external
						  location permissions)
  "Make a Trac environment"
  (declare (ignore name displayname description
		   state identifier parent external
		   location permissions))
  (check-type apache-trac apache-trac)
  (apply #'make-instance 'trac-environment
	 :steward apache-trac
	 (cid::remove-property initargs :apache-trac)))

(defmethod cid:persistent-constructor ((class (eql 'trac-environment)))
  'make-trac-environment)

(defmethod cid:persistent-slots append ((instance trac-environment))
  '((:initarg :apache-trac
     :slot-name steward)
    (:initarg :location
     :slot-name location)
    (:initarg :permissions
     :slot-name permissions)))

(defun trac-environment-name (environment)
  (cond ((stringp environment)
	 environment)
	((typep environment 'trac-environment)
	 (cid:name environment))))

(defun trac-environment-location (environment)
  "The web location of ENVIRONMENT."
  (format nil "/trac/~a" (trac-environment-name environment)))

(defun trac-environment-dirname (environment)
  "The directory name associated with ENVIRONMENT"
  (concatenate 'string (trac-environment-name environment) "/"))
  
(defun trac-environment-directory (environment)
  "The pathname for ENVIRONMENT directory."
  (merge-pathnames (trac-environment-dirname environment)
		   (merge-pathnames #p"environment/" *trac-data-directory*)))

(defun trac-environment-ini-file (environment)
  "The pathname for ENVIRONMENT INI-file."
  (merge-pathnames
   "conf/trac.ini"
   (trac-environment-directory environment)))

(defun trac-environment-chrome-dir (environment)
  "The pathname for ENVIRONMENT's Chrome web data directory."
  (reduce
   #'merge-pathnames
   (list
    #p"htdocs/"
    (trac-environment-dirname environment)
    *trac-www-directory*)
   :from-end t))

(defun trac-environment-wsgi-pathname (environment)
  "The pathname to Trac script."
  (reduce
   #'merge-pathnames
   (list
    #p"cgi-bin/trac.wsgi"
    (trac-environment-dirname environment)
    *trac-www-directory*)
   :from-end t))

(defun trac-environment-site-pathname (environment &optional (suffix "conf"))
  "The pathname for ENVIRONMENT sites configuration."
  (merge-pathnames
   (make-pathname :name (trac-environment-name environment) :type suffix)
   (merge-pathnames #p"sites/" *trac-data-directory*)))

(defun trac-environment-git-directory (environment)
  "The pathname for ENVIRONMENT's git directory."
  (merge-pathnames (trac-environment-dirname environment)
		       *trac-git-directory*))


;;;;
;;;; Trac Admin
;;;;

(defun trac-admin (environment &rest argv)
  "Run ARGV for ENVIRONMENT's trac-admin."
  (run-program
   (list* *trac-admin-pathname*
	  (trac-environment-directory environment)
	  argv)
   :directory *trac-data-directory*
   :identity "www-data"))

(defun trac-environment-apache-configuration-document (environment)
  (let ((parameter-re
	  (let ((identifier
		  '(:greedy-repetition 0 nil
		    (:char-class
		     (:range #\a #\z)
		     (:range #\a #\z)
		     (:range #\0 #\9) #\_)))
		(transformation
		  '(:greedy-repetition 0 nil
		    (:char-class #\c #\l #\u #\x))))
	    `(:sequence
	      #\$ #\{
	      (:register ,identifier)
	      (:greedy-repetition 0 1
	        (:sequence #\: (:register ,transformation)))
	      #\})))
	(parameter-map
	  (list (cons "location"
		      (trac-environment-location environment))
		(cons "chrome"
		      (string-right-trim
		       "/"
		       (namestring (trac-environment-chrome-dir environment))))
		(cons "wsgi"
		      (namestring (trac-environment-wsgi-pathname environment)))
		(cons "auth_name"
		      (cond ((stringp environment)
			     "Trac Authorization")
			    ((typep environment 'trac-environment)
			     (slot-value
			      (slot-value
			       (cid:steward environment)
			       'oidc-configuration)
			      'auth-name))
			    (t
			     (error "Cannot derive AUTH-NAME for ~A" environment))))))
	(template
	  "<Location \"${location}\">
  AuthType openid-connect
  AuthName \"${auth_name}\"
  Require valid-user
</Location>

Alias ${location}/chrome ${chrome}

<Directory \"${chrome}\">
  AuthType openid-connect
  AuthName \"${auth_name}\"
  Require valid-user
</Directory>

WSGIPassAuthorization On
WSGIScriptAlias ${location} ${wsgi}
"))
    (flet ((parameter-expansion (target-string start end match-start match-end reg-starts reg-ends)
             (declare (ignore start end))
             (let*
               ((parameter-name
                  (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))
                (binding
                  (assoc parameter-name parameter-map :test #'string-equal)))
	       (if binding
		   (cdr binding)
		   (subseq target-string match-start match-end)))))
      (ppcre:regex-replace-all parameter-re template #'parameter-expansion))))

;;;; End of file `trac.lisp'
