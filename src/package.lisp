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
   ;; Persistent Objects
   #:*encryption-key*
   #:persistent-constructor
   #:persistent-slots
   #:write-persistent-object
   #:write-persistent-object-to-string
   #:save-persistent-object
   #:read-persistent-object
   #:read-persistent-object-from-string
   #:load-persistent-object
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
   #:steward-class
   #:composite-steward
   #:make-composite-steward
   #:*steward-directory*
   #:with-composite-steward-directory
   ;; Resource
   #:resource
   #:resource-identifier
   #:resource-steward
   #:resource-project
   #:resource-tenant
   #:resource-properties
   #:resource-error
   #:resource-error-operation
   #:resource-error-resource
   #:resource-error-description   
   #:resource-no-longer-exists
   #:resource-already-exists
   #:resource-prerequisite-is-missing
   #:resource-slot-is-immutable
   #:resource-confirmation
   #:with-resource-confirmation
   #:resource-parent
   #:resource-exists-p
   #:resource-ready-p
   #:resource-external-p
   #:resource-internal-p
   #:resource-external
   #:resource-is-external
   #:create-resource
   #:create-missing-prerequisite
   #:delete-resource
   #:update-instance-from-resource
   #:update-resource-from-instance
   #:examine-resource
   #:import-resource
   #:actual-resource
   #:list-resources
   #:list-resource-identifiers
   #:resource-prerequisites
   #:sort-resources
   #:resource-require-p
   #:resource-prerequisite-p
   #:prepare-modification-instructions
   #:apply-modification-instructions
   #:use-resource
   #:recreate-resource
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
   #:content
   ;; Initialization File
   #:read-initialization-file
   #:write-initialization-file
   #:read-initialization-file-from-string
   #:write-initialization-file-to-string
   #:local-initialization-file
   #:make-local-initialization-file
   #:configuration
   ;; Colima
   #:colima-tool
   #:make-colima-tool
   #:colima-instance
   #:make-colima-instance
   ;; Docker Engine
   #:docker-engine
   #:make-docker-engine
   #:docker-volume
   #:make-docker-volume
   #:docker-project
   #:make-docker-project
   ;; Keycloak
   #:keycloak-admin
   #:make-keycloak-admin
   #:get-keycloak-admin-token
   #:keycloak-realm
   #:make-keycloak-realm
   #:keycloak-client
   #:make-keycloak-client
   #:realm
   #:displayname
   #:displayname-html
   #:enabled
   #:brute-force-protected
   #:registration
   #:login-with-email
   #:duplicate-emails
   #:reset-password
   #:edit-username
   ))

(in-package #:org.melusina.cid)

;;;; End of file `package.lisp'
