;;;; build.lisp — Build tools for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/build
  (:use #:common-lisp)
  (:local-nicknames
   (#:cid #:org.melusina.cid))
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
		    "/usr/bin/true"))
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

(defun build-image (image &key (cache t) docker-engine build-time-variables)
  (check-type docker-engine (or cid:docker-engine null))
  (setf image (find-image image))
  (with-slots (repository tag context dockerfile) image
    (unless tag
      (setf tag "latest"))
    (unless build-time-variables
      (setf build-time-variables
	    (list (cons "CID_LINUX_REFERENCE" tag))))
    (unless context
      (setf context #p"."))
    (unless dockerfile
      (setf dockerfile (merge-pathnames #p"Dockerfile" context)))
    (let ((image-tag
	    (concatenate 'string repository '(#\:) tag)))
    (uiop:run-program
     (append
      '("docker")
      (when docker-engine
	(list "--context" (slot-value docker-engine 'cid::context)))
      '("image" "build")
      (unless cache
	(list "--no-cache"))
      (when build-time-variables
	(loop :for (name . value) :in build-time-variables
	      :append (list "--build-arg" (concatenate 'string name "=" value))))
      (list "--file" (namestring dockerfile))
      (list "--tag" image-tag)
      (list (namestring context)))
     :output *standard-output*
     :error-output *trace-output*)
    (finish-output *standard-output*)
    (finish-output *trace-output*)
    (find-image image-tag))))

(defun validate-image (image &key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (setf image (find-image image))
  (eq 0
      (nth-value
       2
       (uiop:run-program
	(append
	 '("docker")
	 (when docker-engine
	   (list "--context" (slot-value docker-engine 'cid::context)))
	 (list
	  "run" "-i" "--rm"
	  "--entrypoint" (image-validate image)
	  (image-name image)))))))

;;;; End of file `build.lisp'
