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
  (:import-from
   #:org.melusina.cid
   #:tenant
   #:project
   #:name
   #:steward
   #:steward-class
   #:displayname
   #:persistent-constructor
   #:persistent-slots
   #:*tenant*
   #:*project*
   #:make-tenant
   #:make-project
   #:find-tenant
   #:find-project
   #:simulation
   #:simulator
   #:remove-property
   #:resource-prerequisites
   #:write-persistent-object
   #:read-persistent-object
   #:sort-resources
   #:create-resource
   #:delete-resource)
  (:export
   #:configure-laboratory
   #:cloud-vendor
   #:make-cloud-vendor
   #:private-network
   #:make-private-network
   #:container-image
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
   #:write-infrastructure-stack
   #:read-infrastructure-stack
   #:make-delivery-stack))

(in-package #:org.melusina.cid/poc)


;;;;
;;;; Cloud Vendor
;;;;

(defclass cloud-vendor (simulator)
  ((credential
    :initarg :credential
    :documentation "The credentials used to access the public API."))
  (:default-initargs
   :name "sws"
   :displayname "Simulated Web Services")
  (:documentation "A cloud vendor providing infrastruture as a service."))

(defun make-cloud-vendor (&rest initargs &key tenant project name displayname 
					      description resource-identifiers 
					      credential)
  "Make a CLOUD-VENDOR with the given parameters."
  (declare (ignore tenant project name displayname
		   description resource-identifiers credential))
  (apply #'make-instance 'cloud-vendor initargs))

(defmethod persistent-constructor ((class (eql 'cloud-vendor)))
  'make-cloud-vendor)

(defmethod persistent-slots append ((instance cloud-vendor))
  '((:credential credential)))


;;;;
;;;; POC Laboratory
;;;;

(defvar *cloud-vendor* nil
  "The CLOUD-VENDOR used to manage resources.")

(defvar *tag* "current"
  "The TAG used to designate resources in a repository.")

(defun configure-laboratory ()
  "Configure POC laboratory.
This sets *TENANT* and *PROJECT* to work on the POC."
  (setf
   *tenant*
   (make-tenant
    :name "local"
    :displayname "Local Tenant")
   *project*
   (make-project
    :name "poc"
    :displayname "Proof of Concept"
    :tenant *tenant*)
   *cloud-vendor*
   (make-cloud-vendor
    :tenant *tenant*
    :project *project*
    :name "sws"
    :displayname "Simulated Web Services"
    :credential "ThisIsNotARealSecret")))


;;;;
;;;; Cloud Private Network
;;;;

(defclass private-network (simulation)
  ((steward-class
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a private network."))

(defun make-private-network (&rest initargs &key cloud-vendor name displayname
						 description state identifier)
  "Make a PRIVATE-NETWORK."
  (declare (ignore name displayname description state identifier))
  (apply #'make-instance 'private-network
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))

(defmethod persistent-constructor ((class (eql 'private-network)))
  'make-private-network)

(defmethod persistent-slots append ((instance private-network))
  '((:cloud-vendor steward)))


;;;;
;;;; Container Image
;;;;

(defclass container-image (simulation)
  ((steward-class
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

(defun make-container-image (&rest initargs &key cloud-vendor
						 name displayname
						 description state identifier
						 repository tag)
  "Make a PRIVATE-NETWORK."
  (declare (ignore name displayname description state identifier repository tag))
  (apply #'make-instance 'container-image
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))

(defmethod persistent-constructor ((class (eql 'container-image)))
  'make-container-image)

(defmethod persistent-slots append ((instance container-image))
  '((:cloud-vendor steward)
    (:repository repository)
    (:tag tag)))


;;;;
;;;; Cloud Container Image Registry
;;;;

(defclass container-image-registry (simulation)
  ((steward-class
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class))
  (:documentation "This class represents a cloud image registry."))

(defun make-container-image-registry (&rest initargs &key cloud-vendor
							  name displayname
							  description state identifier)
  "Make a PRIVATE-NETWORK."
  (declare (ignore name displayname description state identifier))
  (apply #'make-instance 'container-image-registry
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))

(defmethod persistent-constructor ((class (eql 'container-image-registry)))
  'make-container-image-registry)

(defmethod persistent-slots append ((instance container-image-registry))
  '((:cloud-vendor steward)))

(defun find-container-image (&key image-registry repository tag)
  "Find a container image in IMAGE-REGISTRY with the given properties."
  (make-container-image :cloud-vendor (steward image-registry)
			:repository repository
			:tag tag))


;;;;
;;;; Cloud Container Cluster
;;;;

(defclass container-cluster (simulation)
  ((steward-class
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (private-network
    :initarg :private-network
    :reader private-network))
  (:documentation "A cluster providing computational resources to services."))

(defun make-container-cluster (&rest initargs &key cloud-vendor
						   name displayname
						   description state identifier
						   private-network)
  "Make a CONTAINER-CLUSTER."
  (declare (ignore name displayname
		   description state identifier
		   private-network))
  (apply #'make-instance 'container-cluster
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))

(defmethod persistent-constructor ((class (eql 'container-cluster)))
  'make-container-cluster)

(defmethod persistent-slots append ((instance container-cluster))
  '((:cloud-vendor steward)
    (:private-network private-network)))

(defmethod resource-prerequisites append ((instance container-cluster))
  (with-slots (private-network) instance
    (list private-network)))


;;;;
;;;; Cloud Container Service
;;;;

(defclass container-service (simulation)
  ((steward-class
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (cluster
    :initarg :cluster
    :reader cluster)
   (image
    :initarg :image
    :reader image)
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
			       &key cloud-vendor
				    name displayname
				    description state identifier
				    cluster image protocol port)
  "Make a CONTAINER-SERVICE."
  (declare (ignore
	    name displayname
	    description state identifier
	    cluster image protocol port))
  (apply #'make-instance 'container-service
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))


(defmethod persistent-constructor ((class (eql 'container-service)))
  'make-container-service)

(defmethod persistent-slots append ((instance container-service))
  '((:cloud-vendor steward)
    (:cluster cluster)
    (:image image)
    (:protocol protocol)))

(defmethod resource-prerequisites append ((instance container-service))
  (with-slots (cluster image) instance
    (list cluster image)))


;;;;
;;;; Cloud Public Load Balancer
;;;;

(defclass public-load-balancer (simulation)
  ((steward-class
    :type :symbol
    :initform 'cloud-vendor
    :allocation :class)
   (private-network
    :initarg :private-network
    :reader private-network)
   (services
    :initarg :services
    :reader services))
  (:documentation "This class represents a public load balancer."))

(defun make-public-load-balancer (&rest initargs
				  &key cloud-vendor
				       name displayname
				       description state identifier
				       private-network services)
  "Make a CLOUD-PUBLIC-LOADBALANCER."
  (declare (ignore name displayname description state identifier private-network services))
  (apply #'make-instance 'public-load-balancer
	 :steward cloud-vendor
	 (remove-property initargs :cloud-vendor)))


(defmethod persistent-constructor ((class (eql 'public-load-balancer)))
  'make-public-load-balancer)

(defmethod persistent-slots append ((instance public-load-balancer))
  '((:cloud-vendor steward)
    (:private-network private-network)
    (:services services)))

(defmethod resource-prerequisites append ((instance public-load-balancer))
  (with-slots (private-network services) instance
    (append services (list private-network))))



;;;;
;;;; Software Deployment Stack
;;;;

(defclass infrastructure-stack (cid::named-trait)
  ((tenant
    :type tenant
    :initarg :tenant
    :initform *tenant*
    :reader tenant)
   (project
    :type string
    :initarg :project
    :initform *project*
    :reader project)
   (description
    :accessor description
    :type string
    :initarg :description
    :documentation "The DESCRIPTION is used in informational screens
to describe the infrastructure stack.")
   (resources
    :initarg :resources
    :initform nil))
  (:documentation "This class represents a software stack deployment.
An infrastructure stack is a collection of infrastructure resources
defined, provisioned and modified as a unit."))

(defmethod initialize-instance :after ((instance infrastructure-stack)
				       &rest initargs
				       &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((support-initialize-tenant-slot-with-designator ()
	   (cond
	     ((typep (slot-value instance 'tenant) 'string)
	      (with-slots (tenant) instance
		(setf tenant (or (find-tenant tenant)
				 (error "Cannot find tenant ~S." tenant)))))))
	 (support-initialize-project-slot-with-designator ()
	   (cond
	     ((typep (slot-value instance 'project) 'string)
	      (with-slots (tenant project) instance
		(setf project (or (find-project project :tenant tenant )
				  (error "Cannot find project ~S for tenant ~S."
					 project (name tenant))))))))
	 (finalize-resource-list ()
	   (setf (slot-value instance 'resources)
		 (loop :for prerequisite :in (slot-value instance 'resources)
		       :for deep-prerequisites = (resource-prerequisites prerequisite)
		       :append (cons prerequisite deep-prerequisites) :into prerequisites
		       :finally (return (sort-resources (remove-duplicates prerequisites :test #'eq)))))))
    (support-initialize-tenant-slot-with-designator)
    (support-initialize-project-slot-with-designator)
    (finalize-resource-list)))

(defun make-infrastructure-stack (&rest initargs &key tenant project 
						      name displayname
						      description
						      resources)
  "Make an INFRASTRUCTURE-STACK."
  (declare (ignore tenant project name displayname description resources))
  (apply #'make-instance 'infrastructure-stack initargs))

(defmethod persistent-constructor ((class (eql 'infrastructure-stack)))
  'make-infrastructure-stack)

(defmethod persistent-slots append ((instance infrastructure-stack))
  '((:tenant tenant)
    (:project project)
    (:name name)
    (:displayname displayname)
    (:description description)
    (:resources resources)))

(defmethod print-object ((instance infrastructure-stack) stream)
  (flet ((print-readably ()
	   (write-persistent-object instance stream))
	 (print-unreadably ()
	   (with-slots (project tenant name displayname) instance
	     (print-unreadable-object (instance stream :type t :identity t)
	       (format stream "~A:~A:~A ~A"
		       (name tenant) (name project) name displayname)))))
    (if *print-readably*
	(print-readably)
	(print-unreadably))))

(defmethod create-resource ((instance infrastructure-stack))
  (with-slots (resources) instance
    (loop :for resource :in (reverse resources)
	  :do (create-resource resource))))

(defmethod delete-resource ((instance infrastructure-stack))
  (with-slots (resources) instance
    (loop :for resource :in resources
	  :do (delete-resource resource))))


;;;;
;;;; Delivery Stack
;;;;

(defun make-delivery-stack (&key (tenant *tenant*) (project *project*) (tag *tag*) (cloud-vendor *cloud-vendor*))
  (let* ((private-network
	   (make-private-network
	    :cloud-vendor cloud-vendor))
	 (image-registry
	   (make-container-image-registry
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
    (make-infrastructure-stack
     :tenant tenant
     :project project
     :name "delivery"
     :displayname "Delivery Stack"
     :description "A typical infrastructure stack for software delivery."
     :resources (list loadbalancer))))

;;;; End of file `poc.lisp'
