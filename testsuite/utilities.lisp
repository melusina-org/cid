;;;; utilities.lisp — Utilities for El Cid tests

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(defun system-relative-pathname (&optional pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory
	    #.(string-downcase (package-name *package*)))))
    (if pathname
	(merge-pathnames pathname (system-source-directory))
	(system-source-directory))))

(defun enumerate-images (&key tag)
  (loop :for name :in '("cid/linux" "cid/console" "cid/trac" "cid/reverseproxy")
	:for image = (build:find-image name)
	:when tag
	:do (setf (build:image-tag image) tag)
	:collect image))

(defun enumerate-volumes (&key project)
  (loop :for system :in '("trac" "git" "www")
	:collect (docker:make-volume
		  :name (concatenate 'string "cid-" project "-" system))))

(defun testsuite-database-name (&optional(testsuite-id *testsuite-id*))
  "The name of the testsuite database file."
  (cid:user-data-relative-pathname
   "org.melusina.cid"
   "testsuite"
   (make-pathname :name testsuite-id :type "sqlite")))

(defmacro with-test-database (&body body)
  "Run BODY with a connected testsuite database.
The database is reclaimed and destroyed after BODY completes."
  `(let ((cid:*database-type*
	   :sqlite3)
	 (cid:*database-connection-spec*
	   (list (namestring (testsuite-database-name)))))
     (unwind-protect
	  (progn
	    (ensure-directories-exist (testsuite-database-name))
	    (cid:connect-database)
	    ,@body)
       (cid:disconnect-database)
       (clsql:destroy-database cid:*database-connection-spec*
			       :database-type cid:*database-type*))))

;;;; End of file `utilities.lisp'
