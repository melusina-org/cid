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
   #:tenant-scope
   #:ensure-tenant-scope
   ;; Project
   #:project
   #:project-pathname
   #:project-displayname
   #:project-tenant
   #:list-projects
   #:make-project
   #:find-project
  ))

(in-package #:org.melusina.cid)

;;;; End of file `package.lisp'
