;;;; utilities.lisp — Utilities for El Cid Administration Console

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

(defun user-identity (&key (user nil) (numeric nil))
  "Return the USER and GROUP of current process as multiple values.
When NUMERIC is T, numeric values are returned (as strings)."
  (flet ((run-query (&rest command)
	   (uiop:run-program
	    (remove nil command)
	    :output '(:string :stripped t))))
    (let ((user
	    (run-query "/usr/bin/id" (unless numeric "-n") "-u" user))
	  (group
	    (run-query "/usr/bin/id" (unless numeric "-n") "-g" user)))
      (values user group))))

(defun rename-file/unix (pathname new-pathname)
  (uiop:run-program
   (list "sudo" "-u" "root" "/usr/bin/mv" "-f"
	 (namestring pathname)
	 (namestring new-pathname))))

(defun delete-file/unix (pathname)
  (uiop:run-program
   (list "sudo" "-u" "root" "/bin/rm" "-f"
	 (namestring pathname))))

(defun create-empty-file (pathname &key owner group (mode #o600))
  (flet ((query-user-identity-to-provide-defaults ()
	   (when (and owner group)
	     (return-from query-user-identity-to-provide-defaults))
	   (multiple-value-bind (current-user current-group)
	       (user-identity)
	     (unless owner
	       (setf owner current-user))
	     (unless group
	       (setf group current-group))))
	 (create-file ()
	   (uiop:run-program
	    (list "sudo" "-u" "root"
		  "/usr/bin/install"
		  "-o" owner
		  "-g" group
		  "-m" (format nil "~3,'0O" mode)
		  "/dev/null" (namestring pathname)))))
    (query-user-identity-to-provide-defaults)
    (create-file)))

(defun change-file-owner (&key pathname owner)
  (uiop:run-program
   (list "sudo" "-u" "root"
	 "/usr/bin/chown" owner (namestring pathname))))
  
(defun unpredictable-temporary-pathname (pathname)
  (make-pathname
   :name (concatenate 'string
		      "." (pathname-name pathname)
		      (when (pathname-type pathname) ".")
		      (pathname-type pathname))
   :type (ironclad:byte-array-to-hex-string
	  (ironclad:random-data 64))
   :defaults pathname))

(defmacro with-output-to-file ((stream pathname &key owner group (mode #o644)) &body body)
  "The content written to STREAM by BODY is saved to PATHNAME.
The operation is a transaction, which means that either the previous content
of PATHNAME is left untouched or the new content is available."
  (alexandria:with-unique-names (temporary-pathname
				 user-id
				 create-temporary-file
				 remove-temporary-file
				 commit-temporary-file)
    (alexandria:once-only (pathname owner group mode)
      `(let ((,temporary-pathname
	       (unpredictable-temporary-pathname ,pathname))
	     (,user-id
	       ,(user-identity)))
	 (flet ((,create-temporary-file ()
		  (create-empty-file ,temporary-pathname
				     :owner ,user-id
				     :group ,group
				     :mode ,mode)
		  (with-open-file (,stream ,temporary-pathname
					   :direction :output
					   :if-exists :supersede)
		    (change-file-owner :pathname ,temporary-pathname
				       :owner ,owner)
		    ,@body))
		(,remove-temporary-file ()
		  (when (probe-file ,temporary-pathname)
		    (delete-file/unix ,temporary-pathname)))
		(,commit-temporary-file ()
		  (rename-file/unix ,temporary-pathname ,pathname)))
	   (unwind-protect
		(prog1 (,create-temporary-file)
		  (,commit-temporary-file))
	     (,remove-temporary-file)))))))

(defun write-sudoers-policy (&key user run-as command (stream t))
  (format stream "~A ALL = (~A) NOPASSWD: ~A~%"
	  user run-as command))

(defun run-program (command &key directory identity output)
  (let ((command
	  (append
	   (when identity
	     (list "sudo" "-u" identity))
	   command))
	(directory
	  (cond ((stringp directory)
		 directory)
		((pathnamep directory)
		 (namestring directory))
		((null directory)
		 nil)
		(t
		 (error "The value ~S does not designate a directory." directory)))))
    (uiop:with-current-directory (directory)
	  (uiop:run-program command :output output))))

;;;; End of file `utilities.lisp'
