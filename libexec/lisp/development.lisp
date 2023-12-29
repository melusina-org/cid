;;;; development.lisp — Project Development for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/development
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:build #:org.melusina.cid/build)
   (#:colima #:org.melusina.cid/colima))
  (:export
   #:lint
   #+quicklisp
   #:reload
   #:build))

(in-package #:org.melusina.cid/development)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defun system-relative-pathnames (&rest pathnames)
  (mapcar #'system-relative-pathname pathnames))

(defparameter *parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2015–2023")
	(:project-filename . "org.melusina.cid")
        (:project-name . "El Cid")
	(:project-description . "Count of Vivar and Prince of Continuous Integration and Deployment Systems")
        (:project-long-description .
	 #.(concatenate 'string
	    "The **El Cid** project aims at providing a complete continuous"
	    " integration and deployment system that is easy to incrementally"
	    " improve, to share with team mates and collaborators, and that can be"
	    " deployed easily either locally, on bare metal or in the cloud."))
        (:homepage . "https://github.com/melusina-org/cid")
        (:license . :MIT)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathnames
      #p"configure.ac"
      #p"org.melusina.cid.asd"
      #p"doc"
      #p"docker"
      #p"example"
      #p"jenkins"
      #p"jobs"
      #p"src"
      #p"subr"
      #p"support"
      #p"tool"
      #p"libexec/lisp/development.lisp"))))

#+quicklisp
(defun reload ()
  (ql:quickload '("org.melusina.atelier"
		  "org.melusina.confidence"
		  "org.melusina.cid/development")))

(defun build ()
  (dolist (image-name '("linux" "console" "reverseproxy" "gitserver" "trac"))
    (build:build-image (uiop:strcat "cid/" image-name))))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.development:reload)

;;;; End of file `development.lisp'
