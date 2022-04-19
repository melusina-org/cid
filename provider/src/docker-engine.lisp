;;;; docker-engine.lisp — Docker Engine Provider for El Cid

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

;;;;
;;;; Docker Engine Provider
;;;;

(defparameter *docker-engine-pathname*
  #p"/usr/local/bin/docker"
  "The pathname to the docker engine client used to control resources.")

(defclass docker-engine (provider)
  ((pathname
    :initarg :pathname
    :initform "docker-engine"
    :documentation "A name for the PROVIDER.
The fully qualified name of the PROVIDER, which should be a safe Unix path.")
   (description
    :initarg :description
    :initform
    "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client."
    :documentation "A short description of the PROVIDER.")
   (client-pathname
    :initarg :client-pathname
    :initform *docker-engine-pathname*
    :documentation "The pathname to the docker engine client used to control resources.")
   (version
    :initarg :version
    :initform nil
    :documentation "The version information for the docker engine.
This version information is the text returned by the DOCKER VERSION command."))
  (:documentation
   "The class represents a provider creating resources in a docker engine.
The target docker engine is just the docker engine configured for
the `docker' CLI client.

It must operate in `swarm' mode."))

(defmethod describe-object :after ((instance docker-engine) stream)
  (with-slots (client-pathname version) instance
    (format stream "~&Client Pathname: ~A" (namestring client-pathname))
    (format stream "~&Configured: ~A" (when version t)))
  (values))

(defun make-docker-engine (&rest initargs &key pathname description client-pathname)
  "Make a docker engine provider."
  (declare (ignore pathname description client-pathname))
  (or (find-provider :docker-engine)
      (apply #'make-instance 'docker-engine initargs)))


;;;;
;;;; Docker Engine Command
;;;;

(defun docker-engine/command (provider &rest argv)
  (rashell:make-command
   :program (slot-value (find-provider provider) 'client-pathname)
   :documentation "Run docker command"
   :argv argv))

(defun docker-version (&optional (provider :docker-engine))
  (rashell:run-utility (docker-engine/command provider "version") :trim t))


;;;;
;;;; Configure
;;;;

(defmethod configure ((instance docker-engine))
  (with-slots (version) instance
    (unless version
      (multiple-value-bind (stdout stderr) (docker-version instance)
	(declare (ignore stderr))
	(setf version stdout)))))

;;;; End of file `docker-engine.lisp'
