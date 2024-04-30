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
   #:private-network
   #:make-private-network
   #:container-image-registry
   #:make-container-image
   #:container-image-registry
   #:make-container-image-registry
   #:find-container-image
   #:container-cluster
   #:make-container-cluster
   #:container-service
   #:make-container-service
   #:public-load-balancer
   #:make-public-load-balancer
   #:infrastructure-stack
   #:make-infrastructure-stack
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
	:in '(private-network
	      container-image
	      container-image-registry
	      container-cluster
	      container-service
	      public-load-balancer)
	:do (pushnew class cid::*database-application-class-list*)))


;;;;
;;;; Cloud Private Network
;;;;

(clsql:def-view-class private-network (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a private network."))

(defun make-private-network (&rest initargs &key cloud-vendor identifier)
  "Make a PRIVATE-NETWORK."
  (declare (ignore identifier))
  (apply #'make-instance 'private-network
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Container Image
;;;;

(clsql:def-view-class container-image (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (repository
    :initarg :repository
    :type string)
   (tag
    :initarg :tag
    :type string))
  (:documentation "This class represents a cloud image."))

(defun make-container-image (&rest initargs &key cloud-vendor identifier repository tag)
  "Make a PRIVATE-NETWORK."
  (declare (ignore identifier repository tag))
  (apply #'make-instance 'container-image
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))


;;;;
;;;; Cloud Container Image Registry
;;;;

(clsql:def-view-class container-image-registry (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a cloud image registry."))

(defun make-container-image-registry (&rest initargs &key cloud-vendor identifier)
  "Make a PRIVATE-NETWORK."
  (declare (ignore identifier))
  (apply #'make-instance 'container-image-registry
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))

(defun find-container-image (&key image-registry repository tag)
  "Find a container image in IMAGE-REGISTRY with the given properties."
  (make-container-image :cloud-vendor (cid:steward image-registry)
			:repository repository
			:tag tag))


;;;;
;;;; Cloud Container Cluster
;;;;

(clsql:def-view-class container-cluster (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (private-network
    :type string
    :initarg :private-network))
  (:documentation "A cluster providing computational resources to services."))

(defun make-container-cluster (&rest initargs &key cloud-vendor private-network)
  "Make a CONTAINER-CLUSTER."
  (declare (ignore private-network))
  (apply #'make-instance 'container-cluster
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))

(defmethod cid:resource-prerequisites append ((instance container-cluster))
  (with-slots (private-network) instance
    (list private-network)))


;;;;
;;;; Cloud Container Service
;;;;

(clsql:def-view-class container-service (cid:simulation)
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

(defun make-container-service (&rest initargs
				     &key cloud-vendor cluster
					  image protocol port)
  "Make a CONTAINER-SERVICE."
  (declare (ignore cluster image protocol port))
  (apply #'make-instance 'container-service
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))

(defmethod cid:resource-prerequisites append ((instance container-service))
  (with-slots (cluster image) instance
    (list cluster image)))


;;;;
;;;; Cloud Public Load Balancer
;;;;

(clsql:def-view-class public-load-balancer (cid:simulation)
  ((steward-class
    :db-kind :virtual
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (private-network-identifier
    :initarg :private-network-identifier
    :type string)
   (private-network
    :initarg :private-network
    :reader private-network
    :db-kind :join
    :db-info (:join-class private-network
	      :home-key (tenant-name project-name steward-name private-network-identifier)
	      :foreign-key (tenant-name project-name steward-name identifier)
	      :set nil))
   (services
    :initarg :services
    :reader services
    :db-kind :join
    :db-info (:join-class container-service
	      :home-key (tenant-name project-name steward-name identifier)
	      :foreign-key (tenant-name project-name steward-name load-balancer-identifier)
	      :set t)))
  (:documentation "This class represents a public load balancer."))

(defun make-public-load-balancer (&rest initargs &key cloud-vendor private-network services)
  "Make a CLOUD-PUBLIC-LOADBALANCER."
  (declare (ignore private-network services))
  (apply #'make-instance 'public-load-balancer
	 :steward cloud-vendor
	 (cid::remove-property initargs :cloud-vendor)))

(defmethod cid:resource-prerequisites append ((instance public-load-balancer))
  (with-slots (private-network services) instance
    (append services (list private-network))))


;;;;
;;;; Software Deployment Stack
;;;;

(clsql:def-view-class infrastructure-stack ()
  ((resources
    :db-kind :virtual
    :initarg :resources
    :initform nil))
  (:documentation "This class represents a software stack deployment.
An infrastructure stack is a collection of infrastructure resources
defined, provisioned and modified as a unit."))

(defun make-infrastructure-stack (&rest initargs &key resources)
  "Make an INFRASTRUCTURE-STACK."
  (declare (ignore resources))
  (apply #'make-instance 'infrastructure-stack initargs))

(defun save-infrastructure-stack (stack)
  (with-slots (resources) stack
    (loop :for resource :in resources
	  :do (clsql:update-records-from-instance resource)
	  :do (print resource))))

(defun make-poc-stack (&key (tag *tag*) (cloud-vendor *cloud-vendor*))
  (let* ((private-network
	   (make-private-network
	    :cloud-vendor cloud-vendor))
	 (image-registry
	   (make-cloud-container-image-registry
	    :cloud-vendor cloud-vendor))
	 (cluster
	   (make-container-cluster
	    :cloud-vendor cloud-vendor
	    :private-network private-network))
	 (trac-image
	   (find-container-image
	    :image-registry image-registry
	    :repository "cid/trac"
	    :tag tag))
	 (gitserver-image
	   (find-container-image
	    :image-registry image-registry
	    :repository "cid/gitserver"
	    :tag tag))
	 (jenkins-image
	   (find-container-image
	    :image-registry image-registry
	    :repository "cid/jenkins"
	    :tag tag))
	 (trac-service
	   (make-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image trac-image
	    :protocol :http
	    :port 80))
	 (gitserver-service
	   (make-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image gitserver-image
	    :protocol :tcp
	    :port 22))
	 (jenkins-service
	   (make-container-service
	    :cloud-vendor cloud-vendor
	    :cluster cluster
	    :image jenkins-image
	    :protocol :http
	    :port 80))
	 (loadbalancer
	   (make-public-load-balancer
	    :cloud-vendor cloud-vendor
	    :services (list trac-service jenkins-service gitserver-service)
	    :private-network private-network)))
    (make-infrastructure-stack :resources (list loadbalancer))))

;;;; End of file `poc.lisp'
