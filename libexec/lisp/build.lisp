;;;; build.lisp — Build tools for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/build
  (:use #:common-lisp)
  (:local-nicknames
   (#:docker #:org.melusina.cid/docker))
  (:export
   #:image
   #:image-name
   #:image-tag
   #:image-repository
   #:image-context
   #:image-dockerfile
   #:list-images
   #:find-image
   #:build-image
   #:validate-image))

(in-package #:org.melusina.cid/build)

(defun system-relative-pathname (&optional pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory
	    #.(string-downcase (package-name *package*)))))
    (if pathname
	(merge-pathnames pathname (system-source-directory))
	(system-source-directory))))
  
(defparameter *image-directory*
  (system-relative-pathname #p"docker/image/")
  "The pathname to the directory with Docker image definitions.")

(defparameter *registry* nil
  "The registry owning Docker images.")

(defparameter *software-package* "cid"
  "The software package owning our images.")

(defclass image ()
  ((repository
    :initarg :repository
    :initform (error "A REPOSITORY us required.")
    :reader image-repository)
   (tag
    :initarg :tag
    :initform "latest"
    :accessor image-tag)
   (context
    :initarg :context
    :initform (error "A CONTEXT is required.")
    :accessor image-context
    :documentation "The pathname to the Docker CONTEXT used to build.")
   (dockerfile
    :initarg :dockerfile
    :initform (error "A DOCKERFILE is required.")
    :accessor image-dockerfile)
   (validate
    :initarg :validate
    :initform "/usr/bin/true"
    :accessor image-validate)))

(defun image-name (image)
  (with-slots (repository tag) image
    (when (and repository tag)
      (uiop:strcat repository #\: tag))))

(defmethod print-object ((instance image) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (repository) instance
      (format stream ":REPOSITORY ~S" repository))))

(defun list-images (&optional (directory *image-directory*))
  (flet ((make-image (pathname)
	   (let* ((name
		    (car (last (pathname-directory pathname))))
		  (repository
		    (uiop:strcat
		     *software-package*
		     #\/
		     name))
		  (validate
		    (if (string= name "console")
			"/opt/cid/bin/console"
			"/usr/bin/true")))
	     (make-instance
	      'image
	      :repository repository
	      :context (system-relative-pathname)
	      :dockerfile (merge-pathnames #p"Dockerfile" pathname)
	      :validate validate))))
    (mapcar
     #'make-image
     (uiop:subdirectories directory))))

(defun find-image (designator)
  (typecase designator
    (image     
     designator)
    (string
     (find designator (list-images)
	   :key #'image-repository
	   :test #'string=))))

(defun build-image (image &key (cache t))
  (setf image (find-image image))
  (with-slots (repository tag context dockerfile) image
    (docker:create-image
     :dockerfile dockerfile
     :context context
     :repository repository
     :tag tag
     :cache cache)))

(defun validate-image (image)
  (setf image (find-image image))
  (eq 0
      (nth-value
       2
       (uiop:run-program
	(list "docker" "run" "-i" "--rm"
	      "--entrypoint" (image-validate image)
	      (image-name image))))))

;;;; End of file `build.lisp'
