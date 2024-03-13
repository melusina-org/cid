;;;; docker-engine.lisp — Docker-Engine Steward for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defparameter *docker-client-pathname*
  #.(loop :for pathname
	  :in '(#p"/opt/local/bin/docker"
		#p"/usr/bin/docker"
		#p"/usr/local/bin/docker")
	  :when (probe-file pathname)
	  :return pathname)
  "The pathname to the docker engine client used to control resources.")

(clsql:def-view-class docker-engine (steward)
  ((pathname
    :initform "docker-engine"
    :type string
    :db-kind :key
    :db-constraints :not-null)
   (steward-class
    :allocation :class
    :initform 'docker-engine
    :type symbol)
   (contains
    :db-kind :virtual
    :allocation :class
    :initform '(docker-image docker-container docker-volume))
   (description
    :allocation :class
    :initform
    "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."
    :type string)
   (client-pathname
    :type pathname
    :db-type "VARCHAR(255)"
    :initarg :client-pathname
    :initform *docker-client-pathname*
    :documentation "The pathname to the docker engine client
used to control resources.")
   (version
    :type string
    :initarg :version
    :initform nil
    :documentation "The version information for the docker engine.
This version information is the text returned by the DOCKER VERSION command."))
  (:documentation
   "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."))

(defmethod print-object ((instance docker-engine) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (when (and (slot-boundp instance 'pathname)
	       (slot-boundp instance 'project-pathname)
	       (slot-boundp instance 'tenant-pathname))
      (with-slots (pathname project-pathname tenant-pathname) instance
	(format stream "~A ~A ~A" tenant-pathname project-pathname pathname)))
    (with-slots (version client-pathname) instance
      (format stream " ~A ~A" version client-pathname))))

(defun make-docker-engine (&rest initargs &key pathname tenant project client-pathname)
  "Make a docker-engine steward."
  (declare (ignore pathname tenant project client-pathname))
  (apply #'make-instance 'docker-engine initargs))


;;;;
;;;; Docker Engine Command
;;;;

(defun run-docker-engine-query (docker-engine &rest argv)
  (uiop:run-program
   (cons (namestring (slot-value docker-engine 'client-pathname)) argv)
   :output :lines
   :error-output :lines))

(defun docker-version (docker-engine)
  (flet ((version (line)
	   (multiple-value-bind (match-start match-end
				 register-start register-end)
	       (ppcre:scan "^\\s*Version:\\s*(.*)" line)
	     (declare (ignore match-end))
	     (when match-start
	       (subseq line (aref register-start 0) (aref register-end 0))))))
  (loop :for line :in (run-docker-engine-query docker-engine "version")
	:for version = (version line) 
	:when version
	:return version)))
		 

;;;;
;;;; Configure
;;;;

(defmethod configure-steward ((instance docker-engine))
  (with-slots (version) instance
    (unless version
      (setf version (docker-version instance))))
  (values nil))

;;;; End of file `docker-engine.lisp'
