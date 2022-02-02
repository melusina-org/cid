;;;; trac.lisp — Trac integration

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use, 
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

(in-package #:org.melusina.cid)

(defparameter *trac-data-directory* #p"/var/trac/"
  "The directory holding trac state.")

(defparameter *trac-www-directory* #p"/var/trac/www/"
  "The directory holding trac sites.")

(defparameter *trac-git-directory* #p"/var/git/"
  "The directory holding git repositories.")

(defparameter *trac-admin-pathname* #p"/usr/local/bin/trac-admin"
  "Path to the trac-admin utility.")
	      

;;;;
;;;; Validation
;;;;

(defun trac-ensure-valid-installation ()
  "Ensure we are running on a valid trac installation.
If we cannot validate that we are running on a valid trac installation,
then an error is signaled, otherwise NIL is returned."
  (let ((required-directories (list *trac-data-directory*
				    *trac-www-directory*
				    *trac-git-directory*)))
    (loop :for directory :in required-directories
	  :do (unless (probe-file directory)
	       (error "The directory ~a cannot be read.
This directory is required in a valid trac installation but it could not
be read on this system." directory)))))



;;;;
;;;; Trac Environment
;;;;

(defun trac-environment-location (environment)
  "The web location of ENVIRONMENT."
  (format nil "/trac/~a" environment))

(defun trac-environment-permission-db (environment)
  "The permission database for ENVIRONMENT."
  (declare (ignore environment))
  '(("admin" . ("TRAC_ADMIN"))
    ("developer" . ("WIKI_ADMIN" "REPORT_ADMIN" "TICKET_MODIFY"))))


(defun trac-environment-dirname (environment)
  "The directory name associated with ENVIRONMENT"
  (concatenate 'string environment "/"))


(defun trac-environment-directory (&optional environment)
  "The pathname for environment directory.
When ENVIRONMENT is not provided, the directory holding all environment
directories is returned."
  (let ((basedir (merge-pathnames #p"environment/" *trac-data-directory*)))
    (if environment
	(merge-pathnames (trac-environment-dirname environment) basedir)
	basedir)))

(defun trac-ini-file (environment)
  "The pathname for ENVIRONMENT INI-file."
  (merge-pathnames
   "conf/trac.ini"
   (trac-environment-directory environment)))


(defun trac-www-directory (&optional environment)
  "The pathname for ENVIRONMENT's web directory.
When environment is not provided, the directory holding all web files is returned."
  (if environment
      (merge-pathnames (trac-environment-dirname environment) *trac-www-directory*)
      *trac-www-directory*))


(defun trac-git-directory (&optional environment)
  "The pathname for ENVIRONMENT's git directory.
When ENVIRONMENT is not provided, the directory holding all git repositories is returned."
  (if environment
      (merge-pathnames (trac-environment-dirname environment) *trac-git-directory*)
      *trac-git-directory*))


(defun trac-sites-directory ()
  "The pathname for sites directory."
  (merge-pathnames #p"sites/" *trac-data-directory*))


(defun trac-site-pathname (environment suffix)
  "The pathname for filename."
  (reduce #'merge-pathnames
	  (list (make-pathname :name environment :type suffix) #p"sites/" *trac-data-directory*)
	  :from-end t))

(defun trac-admin (environment &rest argv)
  "Run ARGV for ENVIRONMENT's trac-admin."
  (let ((command
	  (format nil "~{~a~^ ~}"
		  (list* *trac-admin-pathname*
			 (trac-environment-directory environment)
			 argv))))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/bin/su"
		    :directory (trac-environment-directory)
		    :argv (list "-l" "www-data" "-s" "/bin/sh" "-c" command)))))


(defun trac-list-environments ()
  "The list of trac environments configured in this deployment."
  (labels
      ((environment-of-directory (directoryname)
	 (ppcre:regex-replace ".*/" directoryname ""))
       (find-environments ()
	 (make-instance 'rashell:command
			:program #p"/usr/bin/find"
			:directory (trac-environment-directory)
			:argv '("." "-maxdepth" "1" "-mindepth" "1")
			:object-of-output-line #'environment-of-directory)))
    (if (probe-file (trac-environment-directory))
	(rashell:run-query (find-environments))
	nil)))


(defun trac-find-environment (environment)
  "Find trac ENVIRONMENT.
This returns the environment called ENVIRONMENT or NIL if no environment
with this name does exist."
  (when (probe-file (trac-environment-directory environment))
    environment))


(defun trac-create-environment/apache-document (environment)
  (let ((parameter-re
	  '(:sequence
	    #\$ #\{
	    (:register
	     (:greedy-repetition 0 nil
	      (:char-class (:range #\a #\z) (:range #\a #\z) (:range #\0 #\9) #\_)))
	    (:greedy-repetition 0 1
	     (:sequence #\:
	      (:register (:greedy-repetition 0 nil (:char-class #\c #\l #\u #\x)))))
	    #\}))
	(parameter-map
	  (list (cons "location" (trac-environment-location environment))
		(cons "tracdir" (string-right-trim "/" (namestring *trac-data-directory*)))
		(cons "environment" environment)))
	(template
	  "alias ${location}/chrome ${tracdir}/www/${environment}/htdocs

<directory \"${tracdir}/www/${environment}/htdocs\">
  <ifmodule mod_authz_core.c>
    require all granted
  </ifmodule>
</directory>

<location \"${location}/login\">
  authtype basic
  authname \"trac ${environment}\"
  authuserfile ${tracdir}/sites/${environment}.htpasswd
  require valid-user
</location>

wsgiscriptalias ${location} ${tracdir}/www/${environment}/cgi-bin/trac.wsgi
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


(defun trac-create-environment/tracini-document (environment)
  (declare (ignore environment))
  "[components]
tracopt.versioncontrol.git.* = enabled
")


(defun trac-create-environment (environment)
  "Create ENVIRONMENT."
  (when (trac-find-environment environment)
    (error "An environment called ~a already exists." environment))
  (bsd-install-directory
   (list
    (trac-www-directory environment)
    (trac-environment-directory environment))
   :owner "www-data"
   :group "www-data"
   :mode #o750)
  (bsd-install-directory
   (trac-git-directory environment)
   :owner "git"
   :group "git"
   :mode #o750)
  (trac-admin environment "initenv" environment "sqlite:db/trac.db")
  (with-open-file (stream (trac-ini-file environment) :direction :output :if-exists :append)
    (write-string (trac-create-environment/tracini-document environment) stream))
  
  (rashell:run-utility
   (make-instance 'rashell:command
		  :program #p"/bin/chown"
		  :argv (list "www-data:www-data" (namestring (trac-www-directory)))))
  (trac-admin environment "deploy" (trac-www-directory environment))
  (bsd-install-empty-file (trac-site-pathname environment "htpasswd")
			  :owner "www-data"
			  :group "www-data"
			  :mode #o640)
  (bsd-install-empty-file (trac-site-pathname environment "conf")
			  :owner "www-data"
			  :group "www-data"
			  :mode #o640)
  (with-open-file (stream (trac-site-pathname environment "conf")
			  :direction :output
			  :if-exists :supersede)
    (write-string (trac-create-environment/apache-document environment) stream))
  (loop for permission in (trac-environment-permission-db environment)
	do (apply #'trac-admin environment "permission" "add" (car permission) (cdr permission))))


(defun trac-delete-environment (environment)
  "Delete ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (rashell:rm (list (trac-site-pathname environment "conf")
		    (trac-site-pathname environment "htpasswd")
		    (trac-www-directory environment)
		    (trac-environment-directory environment)
		    (trac-git-directory environment))
	      :recursive t
	      :force t)
  (values nil))

(defun trac-list-users (environment)
  "The list of users for ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (when (probe-file (trac-site-pathname environment "htpasswd"))
    (rashell:run-query
     (make-instance 'rashell:command
		    :program #p"/usr/bin/awk"
		    :argv (list "-F" ":" "
{users[$1]}
END {
  for(user in users) {
    print(user)
  }
}" (namestring (trac-site-pathname environment "htpasswd")))))))


(defun trac-create-user (environment username role password)
  "Create USERNAME with given ROLE in ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (unless (find role (mapcar #'car (trac-environment-permission-db environment)) :test #'string=)
    (error "A role called ~a does not exist in environment ~a." role environment))
  (unless (ppcre:scan "[a-z][a-z0-9_]*" username)
    (error "A username must consist of safe lowercase characters and underscores."))
  (trac-admin environment "permission" "add" username role)
  (rashell:run-command
   (make-instance 'rashell:command
		  :program #p"/usr/bin/htpasswd"
		  :argv (list "-b" "-b"
			      (namestring (trac-site-pathname environment "htpasswd"))
			      username
			      password)))
  (values nil))


(defun trac-delete-user (environment username)
  "Delete USERNAME in ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (rashell:run-command
   (make-instance 'rashell:command
		  :program #p"/usr/bin/htpasswd"
		  :argv (list "-d"
			      (namestring (trac-site-pathname environment "htpasswd"))
			      username)))
  (trac-admin environment "permission" "remove" username)
  (values nil))


(defun trac-dump (dumpname)
  "Dump trac data and repositories to a tarball whose names is built from DUMPNAME."
  (let ((tmpdir
	  (merge-pathnames (concatenate 'string (pathname-name dumpname) "/")
			   #p"/var/tmp/trac/"))
	(tarballname
	  (concatenate 'string (namestring dumpname) ".trac.txz")))
    (labels
	((dump (srcdir dumpdir &optional files)
	   (let ((script
		   (format nil "find ~a | cpio -dump ~a"
			   (or files ".")
			 (namestring (merge-pathnames dumpdir tmpdir)))))
	     (rashell:run-utility
	      (make-instance 'rashell:command
			     :directory srcdir
			     :program #p"/bin/sh"
			     :argv (list "-c" script))))))
      (bsd-install-directory
       (list
	(merge-pathnames #p"trac/" tmpdir)
	(merge-pathnames #p"trac/environments/" tmpdir)
	(merge-pathnames #p"trac/sites/" tmpdir)
	(merge-pathnames #p"trac/www/" tmpdir))
       :owner "www-data"
       :group "www-data"
       :mode #o750)
      (bsd-install-directory
       (merge-pathnames #p"git/" tmpdir)
       :owner "git"
       :group "git"
       :mode #o750)
      (dump *trac-git-directory* "git")
      (dump *trac-data-directory* "trac" "sites")
      (dump *trac-data-directory* "trac" "www")
      (loop for environment in (trac-list-environments)
	    do (trac-admin environment
			   "hotcopy"
			   (namestring
			    (let ((*trac-data-directory* (merge-pathnames #p"trac/" tmpdir)))
			      (trac-environment-directory environment)))))

      (rashell:run-utility
       (make-instance 'rashell:command
		      :directory tmpdir
		      :program #p"/bin/tar"
		      :argv (list "cJf" tarballname ".")))
      (rashell:rm tmpdir :recursive t :force t)
      (values nil))))


(defun trac-restore (dumpname)
  "Restore trac data and repositories from a tarball DUMPNAME."
  (let ((tarballname
	  (concatenate 'string (namestring dumpname) ".trac.txz")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :directory *trac-data-directory*
		    :program #p"/bin/tar"
		    :argv (list "xJf" tarballname "--strip-components" "2" "./trac/")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :directory *trac-git-directory*
		    :program #p"/bin/tar"
		    :argv (list "xJf" tarballname "--strip-components" "2" "./git/")))
    (values nil)))


(defun trac-add-ssh-authorized-key (authorized-key)
  "Add AUTHORIZED-key to the file of authorized keys to access git server."
  (let ((ssh-directory
	  (merge-pathnames #p".ssh/" *trac-git-directory*))
	(ssh-authorized-keys
	  (merge-pathnames #p".ssh/authorized_keys" *trac-git-directory*))
	(safe-authorized-key
	  (concatenate 'string (string-right-trim '(#\newline) authorized-key) '(#\newline))))
    (unless (probe-file ssh-directory)
      (bsd-install-directory ssh-directory :owner "git" :group "git" :mode #o700))
    (unless (probe-file ssh-authorized-keys)
      (bsd-install-empty-file ssh-authorized-keys :owner "git" :group "git" :mode #o600))
    (with-open-file (stream ssh-authorized-keys :direction :output :if-exists :append)
      (write-string safe-authorized-key stream))))


(defun trac-create-git-repository (environment repository)
  "Create a git REPOSITORY in the trac ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (let ((repository-name
	  repository)
	(repository-pathname
	  (merge-pathnames
	   (if (ppcre:scan ".*[.]git" repository)
	       (concatenate 'string repository "/")
	       (concatenate 'string repository ".git/"))
	   (trac-git-directory environment)))
	(environment-directory
	  (namestring (trac-environment-directory environment))))
    (bsd-install-directory repository-pathname :owner "git" :group "git" :mode #o750)
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/bin/su"
		    :argv (list "-l" "git" "-s" "/bin/sh" 
				"-c" (concatenate 'string "/usr/bin/git init --bare " (namestring repository-pathname)))))
    (rashell:rm (merge-pathnames "hooks/post-receive" repository-pathname) :force t)
    (rashell:ln #p"/opt/cid/libexec/cid/cid_githook_postreceive"
		(merge-pathnames "hooks/post-receive" repository-pathname)
		:symbolic t)
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/usr/bin/git"
		    :directory repository-pathname
		    :argv (list "config" "trac.addchangeset" "true")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/usr/bin/git"
		    :directory repository-pathname
		    :argv (list "config" "trac.environment" environment-directory)))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/usr/bin/git"
		    :directory repository-pathname
		    :argv (list "config" "trac.repositoryname" repository-name)))
    (trac-admin environment "repository" "add" repository (namestring repository-pathname) "git")
    (values nil)))


(defun trac-delete-git-repository (environment repository)
  "Delete the git REPOSITORY from the trac ENVIRONMENT."
  (unless (trac-find-environment environment)
    (error "An environment called ~a does not exist." environment))
  (let ((repository-pathname
	  (merge-pathnames
	   (if (ppcre:scan ".*[.]git" repository)
	       (concatenate 'string repository "/")
	       (concatenate 'string repository ".git/"))
	   (trac-git-directory environment))))
    (rashell:run-utility
     (rashell:rm repository-pathname :force t :recursive t))
    (values nil)))

;;;; End of file `trac.lisp'
