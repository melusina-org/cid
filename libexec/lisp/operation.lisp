;;;; operation.lisp — Project Operation for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/operation
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:docker #:org.melusina.cid/docker))
  (:export
   #:lint
   #+quicklisp
   #:reload
   #:build
   
   ;; Project
   #:project
   #:project-name
   #:project-status
   #:project-created-p
   #:make-project
   #:list-projects
   #:create-project
   #:update-project
   #:start-project
   #:stop-project
   #:find-project
   #:delete-project
   ))

(in-package #:org.melusina.cid/operation)

(defun system-relative-pathname (pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory #.(string-downcase (package-name *package*)))))
    (merge-pathnames pathname (system-source-directory))))

(defparameter *docker-compose*
  (system-relative-pathname #p"docker/compose/cid.yml")
  "The DOCKER-COMPOSE file used to run El Cid in the laboratory.")

(defclass project ()
  ((name
    :initarg :name
    :initform (error "A NAME is required.")
    :reader project-name)
   (docker-compose
    :initform *docker-compose*
    :reader project-docker-compose)
   (status
    :initarg :status
    :initform nil
    :reader project-status)
   (volumes
    :initform nil)))

(defun project-created-p (project)
  (slot-value project 'status))

(defmethod initialize-instance :after ((instance project) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (volumes name) instance
    (setf volumes
 	  (loop :for system :in '("trac" "git" "www")
		:collect (docker:make-volume
			  :name (concatenate 'string "cid-" name "-" system))))))

(defun make-project (&rest initargs &key name status docker-compose)
  (declare (ignore name status docker-compose))
  (apply #'make-instance 'project initargs))

(defmethod print-object ((instance project) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (name status) instance
      (format stream ":NAME ~S :STATUS ~A" name status))))

(defparameter *project*
  (make-project :name "cid"))

(defun list-projects ()
  (flet ((project-name (volume-name)
	   (multiple-value-bind (match-start match-end reg-starts reg-ends)
	       (ppcre:scan "cid-([^-]+)-git" volume-name)
	     (declare (ignore match-end))
	     (when match-start
	       (subseq volume-name (aref reg-starts 0) (aref reg-ends 0))))))
    (loop :for volume :in (docker:list-volumes)
	  :for project-name = (project-name (docker:volume-name volume))
	  :when project-name
	  :collect (make-project :name project-name :status t))))

(defun find-project (designator)
  "Find project designated by DESIGNATOR."
  (typecase designator
    (project     
     designator)
    (string
     (find designator (list-projects)
	   :key #'project-name
	   :test #'string=))))

(defun update-project (&optional (project *project*))
  "Update PROJECT slots from its actual state."
  (let ((actual-state
	  (find-project (project-name project))))
    (unless actual-state
      (setf (slot-value project 'status) nil)
      (return-from update-project project))
    (loop :for slot-name :in '(status volumes)
	  :do (setf (slot-value project slot-name)
		    (slot-value actual-state slot-name)))
    (loop :for volume :in (slot-value project 'volumes)
	  :do (docker:update-volume volume))
    (values project)))

(defun create-project (&key name (docker-compose *docker-compose*))
  (let ((project
	  (make-project
	   :name name
	   :docker-compose docker-compose)))
    (with-slots (volumes status) project
      (when status
	(return-from create-project project))
      (loop :for volume :in volumes
	    :do (docker:create-volume
		 :name (docker:volume-name volume)
		 :driver (docker:volume-driver volume)))
      (setf status t)
      (values project))))

(defun start-project (&optional (project *project*))
  (let ((saved-project
	  (uiop:getenv "cid_project")))
    (setf (uiop:getenv "cid_project")
	  (project-name project))
    (unwind-protect
	 (uiop:run-program
	  (list "docker-compose"
		"--project-name" (project-name project)
		"--file" (namestring
			  (project-docker-compose project))
		"up" "--detach")
	  :output t
	  :error-output t)
      (when saved-project
	(setf
	 (uiop:getenv "cid_project")
	 saved-project)))))

(defun stop-project (&optional (project *project*))
  (let ((saved-project
	  (uiop:getenv "cid_project")))
    (setf (uiop:getenv "cid_project")
	  (project-name project))
    (unwind-protect
	 (uiop:run-program
	  (list "docker-compose"
		"--project-name" (project-name project)
		"--file" (namestring
			  (project-docker-compose project))
		"down")
	  :output t
	  :error-output t)
      (when saved-project
	(setf
	 (uiop:getenv "cid_project")
	 saved-project)))))

(defun delete-project (&optional (project *project*))
  (let ((saved-project
	  (uiop:getenv "cid_project")))
    (setf (uiop:getenv "cid_project")
	  (project-name project))
    (unwind-protect
	 (progn
	   (uiop:run-program
	    (list "docker-compose"
		  "--project-name" (project-name project)
		  "--file" (namestring
			    (project-docker-compose project))
		  "rm")
	    :output t
	    :error-output t)
	   (with-slots (volumes status) project
	     (loop :for volume :in volumes
		   :do (docker:delete-volume volume))))
      (when saved-project
	(setf
	 (uiop:getenv "cid_project")
	 saved-project)))))

;;;; End of file `operation.lisp'
