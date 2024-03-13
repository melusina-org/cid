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
   ;; Named Trait
   #:name
   #:displayname
   ;; Tenant
   #:tenant
   #:list-tenants
   #:make-tenant
   #:find-tenant
   ;; Project
   #:project
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
   #:examine-differences-between-instance-and-resource))

(in-package #:org.melusina.cid)

;;;; End of file `package.lisp'
