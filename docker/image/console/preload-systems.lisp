;;;; preload-systems.lisp — Preload Systems for QuickLisp

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

(defparameter *local-projects*
  #P"/opt/cid/var/quicklisp/local-projects/")

(defparameter *preload-systems*
  '((:system #:alexandria)
    (:system #:cl-ppcre)
    (:system #:drakma)
    (:system #:flexi-streams)
    (:system #:ironclad)
    (:system #:yason)
    (:system #:uuid)
    (:system #:swank)
    (:system #:org.melusina.confidence
     :repository "https://github.com/melusina-org/cl-confidence.git")
    (:system #:org.melusina.atelier
     :repository "https://github.com/melusina-org/cl-atelier.git")
    (:system #:org.melusina.rashell
     :repository "https://github.com/melusina-org/cl-rashell.git")
    (:system #:org.melusina.webmachine
     :repository "https://github.com/melusina-org/cl-webmachine.git")))


(defun working-copy (repository)
  (flet ((trim-final-dotgit (string)
           (subseq string 0 (search ".git" string :from-end t)))
         (trim-initial-location (string)
           (subseq
            string
            (or
             (1+ (position #\Slash string :from-end t))
             0)))
         (trim-initial-cl-hyphen (string)
           (if (eq 0 (search "cl-" string))
               (subseq string 3)
               string)))
    (merge-pathnames
     (reduce #'(lambda (x f) (funcall f x))
             (list #'trim-final-dotgit
                   #'trim-initial-location
		   #'trim-initial-cl-hyphen)
             :initial-value repository)
     *local-projects*)))
  
(defun preload-systems (systems)
  (flet ((local-projects-p ()
	   (some (lambda (system)
		   (getf system :repository))
		 systems))
	 (clone-repository (repository)
	   (uiop:run-program
	    (list "git" "clone" repository
		  (namestring (working-copy repository)))))
	 (preload-system (system)
	   (format *trace-output*
		   "Preload system ~A from ~A.~&"
		   system
		   (ql:where-is-system system))
	   (ql:quickload system)))
    (loop :for system :in systems
	  :for repository = (getf system :repository)
	  :when repository
	  :do (clone-repository repository))
    (when (local-projects-p)
      (ql:register-local-projects))
    (loop :for system :in systems
	  :for system-name = (getf system :system)
	  :when system-name
	  :do (preload-system system-name))))

(progn (preload-systems *preload-systems*))

;;;; End of file `preload-systems.lisp'
