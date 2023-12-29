;;;; package.lisp — Package for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid
  (:use #:common-lisp)
  (:export
   
   ;; Database
   #:*database-type*
   #:*database-connection-spec*
   #:user-data-relative-pathname
   #:connect-database
   #:disconnect-database
   #:with-database
   ;; Tenant
   #:tenant
   #:tenant-pathname
   #:tenant-displayname
   #:list-tenants
   #:make-tenant
   #:find-tenant
   ;; Project
   #:project
   #:project-pathname
   #:project-displayname
   #:project-tenant
   #:list-projects
   #:make-project
   #:find-project
   ;; Steward
   #:steward
   #:steward-pathname
   #:steward-project
   #:steward-tenant
   #:find-steward
   #:configure-steward
   ;; Resource
   #:resource
   #:resource-pathname
   #:resource-steward
   #:resource-project
   #:resource-tenant
   #:resource-properties
   #:list-resources
   #:find-resource
   #:compute-instance-resource-differences
   #:update-instance-from-resource
   #:update-resource-from-instance
   ;; Empty Steward
   #:empty
   #:make-empty
   ;; Properties
   #:property
   #:property-name
   #:property-value
   #:property-list
   #:make-property-list
   ;; Filesystem Subtree
   #:filesystem-subtree
   #:make-filesystem-subtree
   ;; Docker Engine
   #:docker-engine
   #:make-docker-engine
   #:docker-image
   #:docker-container
   #:docker-volume
   ;; MacOS X Security
   #:macos-security
   #:make-macos-security))

(in-package #:org.melusina.cid)

;;;; End of file `package.lisp'
