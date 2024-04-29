;;;; poc.lisp — POC Composable Infrastructure Stacks for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/poc
  (:use #:common-lisp)
  (:local-nicknames
   (#:cid #:org.melusina.cid))
  (:export
   #:cloud-vendor
   #:make-cloud-vendor
   #:cloud-private-network
   #:private-network
   #:container-image-registry
   #:make-container-image-registry
   #:find-cloud-container-image
   #:cloud-container-cluster
   #:make-cloud-container-cluster
   #:cloud-container-service
   #:make-cloud-container-service
   #:cloud-public-load-balancer
   #:make-cloud-public-load-balancer
   #:stack
   #:make-stack
   #:make-poc-stack))

(in-package #:org.melusina.cid/poc)


;;;;
;;;; Cloud Vendor
;;;;

(clsql:def-view-class cloud-vendor (cid:simulator)
  ((credential
    :initarg :credential
    :documentation "The credentials used to access the public API."))
  (:documentation "A cloud vendor providing infrastruture as a service."))

(defun make-cloud-vendor (&rest initargs &key tenant project name displayname credential)
  "Make a CLOUD-VENDOR with the given parameters."
  (declare (ignore tenant project name displayname credential))
  (apply #'make-instance 'cloud-vendor initargs))


;;;;
;;;; POC Laboratory
;;;;

(defvar *cloud-vendor* nil
  "The CLOUD-VENDOR used to manage resources.")

(defvar *tag* "current"
  "The TAG used to designate resources in a repository.")

(defun configure-laboratory ()
  "Configure POC laboratory.
This sets CID:*TENANT* and CID:*PROJECT* to work on the POC."
  (setf
   cid:*tenant*
   (cid:make-tenant
    :name "local"
    :displayname "Local Tenant")
   cid:*project*
   (cid:make-project
    :name "poc"
    :displayname "Proof of Concept"
    :tenant cid:*tenant*)
   *cloud-vendor*
   (make-cloud-vendor
    :tenant cid:*tenant*
    :project cid:*project*
    :name "sws"
    :displayname "Simulated Web Services"
    :credential "ThisIsNotARealSecret"))
  (loop :for class
	:in '(cloud-private-network
	      cloud-container-image-registry
	      cloud-container-cluster
	      cloud-container-service
	      cloud-public-load-balancer
	      stack)
	:do (pushnew class cid::*database-application-class-list*)))


;;;;
;;;; Cloud Private Network
;;;;

(clsql:def-view-class cloud-private-network (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a private network."))

(defun make-cloud-private-network (&rest initargs &key cloud-vendor identifier)
  "Make a CLOUD-PRIVATE-NETWORK."
  (declare (ignore identifier))
  (apply #'make-instance 'cloud-private-network
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Cloud Container Image Registry
;;;;

(clsql:def-view-class cloud-container-image-registry (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a cloud image registry."))

(defun make-cloud-container-image-registry (&rest initargs &key cloud-vendor identifier)
  "Make a CLOUD-PRIVATE-NETWORK."
  (declare (ignore identifier))
  (apply #'make-instance 'cloud-container-image-registry
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))

(defun find-cloud-container-image (&key image-registry repository tag)
  "Find a container image in IMAGE-REGISTRY with the given properties."
  (declare (ignore image-registry repository tag))
  (cerror "Continue" "Not implemented.")
  (values "Some image"))


;;;;
;;;; Cloud Container Cluster
;;;;

(clsql:def-view-class cloud-container-cluster (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (private-network
    :type string
    :initarg :private-network))
  (:documentation "A cluster providing computational resources to services."))

(defun make-cloud-container-cluster (&rest initargs &key cloud-vendor private-network)
  "Make a CLOUD-CONTAINER-CLUSTER."
  (declare (ignore private-network))
  (apply #'make-instance 'cloud-container-cluster
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Cloud Container Service
;;;;

(clsql:def-view-class cloud-container-service (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (cluster
    :type string
    :initarg :cluster)
   (image
    :type string
    :initarg :image
    :documentation "The container image providing the application.")
   (protocol
    :type keyword
    :initarg :protocol
    :documentation "The PROTOCOL used by the service.
Allowed values are one of :HTTP, :HTTPS, :TCP.")
   (port
    :type integer
    :initarg :port
    :documentation "The network PORT used by the service."))
  (:documentation "This class represents a cloud container service."))

(defun make-cloud-container-service (&rest initargs
				     &key cloud-vendor cluster
					  image protocol port)
  "Make a CLOUD-CONTAINER-SERVICE."
  (declare (ignore cluster image protocol port))
  (apply #'make-instance 'cloud-container-service
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Cloud Public Load Balancer
;;;;

(clsql:def-view-class cloud-public-load-balancer (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a public load balancer."))

(defun make-cloud-public-load-balancer (&rest initargs &key cloud-vendor private-network rules)
  "Make a CLOUD-PUBLIC-LOADBALANCER."
  (declare (ignore private-network rules))
  (apply #'make-instance 'cloud-public-load-balancer
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Software Deployment Stack
;;;;

(clsql:def-view-class stack (cid:simulation)
  ((resources
    :db-kind :virtual
    :initarg :resources
    :initform nil))
  (:documentation "This class represents a software stack deployment.
An infrastructure stack is a collection of infrastructure resources
defined, provisioned and modified as a unit."))

(defun make-stack (&rest initargs &key resources)
  "Make an infrastructure STACK."
  (declare (ignore resources))
  (apply #'make-instance 'stack initargs))

(defun make-poc-stack (&key (tag *tag*) (cloud-vendor *cloud-vendor*))
  (let* ((private-network
	   (make-cloud-private-network
	    :cloud-vendor cloud-vendor))
	 (image-registry
	   (make-cloud-container-image-registry
	    :cloud-vendor cloud-vendor))
	 (cluster
	   (make-cloud-container-cluster
	    :cloud-vendor cloud-vendor
	    :private-network private-network))
	 (trac-image
	   (find-cloud-container-image
	    :image-registry image-registry
	    :repository "cid/trac"
	    :tag tag))
	 (gitserver-image
	   (find-cloud-container-image
	    :image-registry image-registry
	    :repository "cid/gitserver"
	    :tag tag))
	 (jenkins-image
	   (find-cloud-container-image
	    :image-registry image-registry
	    :repository "cid/jenkins"
	    :tag tag))
	 (trac-service
	   (make-cloud-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image trac-image
	    :protocol :http
	    :port 80))
	 (gitserver-service
	   (make-cloud-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image gitserver-image
	    :protocol :tcp
	    :port 22))
	 (jenkins-service
	   (make-cloud-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image jenkins-image
	    :protocol :http
	    :port 80))
	 (loadbalancer
	   (make-cloud-public-load-balancer
	    :cloud-vendor cloud-vendor
	    :rules (list
		    (list
		     :port 80
		     :protocol :http
		     :action :redirect)
		    (list
		     :port 443
		     :protocol :https
		     :host "trac.*"
		     :action trac-service)
		    (list
		     :port 443
		     :protocol :https
		     :host "jenkins.*"
		     :action jenkins-service)
		    (list
		     :port 22
		     :protocol :tcp
		     :host "git.*"
		     :action gitserver-service)))))
    (make-stack :resources (list loadbalancer))))

;;;; End of file `poc.lisp'
