;;;; package.lisp — Package for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid
  (:use #:common-lisp)
  (:export
   
   ;; Utilities
   #:user-data-relative-pathname
   ;; Common Accessors
   #:description
   ;; Named Trait
   #:name
   #:displayname
   ;; Tenant
   #:*tenant*
   #:*tenant-directory*
   #:tenant
   #:tenant-name
   #:list-tenants
   #:make-tenant
   #:find-tenant
   ;; Project
   #:*project*
   #:*project-directory*
   #:project
   #:project-name
   #:list-projects
   #:make-project
   #:find-project
   ;; Steward
   #:steward
   #:find-steward
   #:configure-steward
   ;; Resource
   #:resource
   #:resource-identifier
   #:resource-pathname
   #:resource-steward
   #:resource-project
   #:resource-tenant
   #:resource-properties
   #:resource-error
   #:resource-no-longer-exists
   #:resource-confirmation
   #:with-resource-confirmation
   #:resource-exists-p
   #:resource-ready-p
   #:create-resource
   #:delete-resource
   #:update-instance-from-resource
   #:update-resource-from-instance
   #:examine-resource
   #:import-resource
   #:list-resources
   #:list-resource-identifiers
   #:examine-differences-between-instance-and-resource
   #:resource-prerequisites
   #:resource-serial
   ;; Infrastructure Stacks
   #:stack-name
   ;; Simulators and Simulations
   #:simulator
   #:make-simulator
   #:simulation
   #:make-simulation
   ;; Local Filesystem Subtree
   #:local-filesystem-subtree
   #:make-local-filesystem-subtree
   #:local-text-file
   #:make-local-text-file
   ;; Docker Engine
   #:docker-engine
   #:make-docker-engine
   #:docker-volume
   #:make-docker-volume
   #:docker-image
   #:make-docker-image
   ))

(in-package #:org.melusina.cid)

;;;; End of file `package.lisp'
