;;;; package.lisp — Package for El Cid

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

(defpackage #:org.melusina.cid
  (:nicknames #:cid)
  (:use #:cl)
  (:import-from :alexandria :define-constant :ensure-list :make-keyword)
  (:export
   ;;; Random Strings
   #:*alphabet-hexadecimal*
   #:*alphabet-base36*
   #:*alphabet-base64*
   #:random-string
   ;;; Tenant
   #:make-tenant
   #:find-tenant
   #:tenant-pathname
   #:tenant-displayname
   #:tenant-scope
   #:ensure-tenant-scope
   ;;; Project
   #:make-project
   #:find-project
   #:project-pathname
   #:project-displayname
   #:project-tenant
   ;;; Identity
   #:make-user
   #:find-user
   #:user-pathname
   #:user-displayname
   #:user-tenant
   #:user-role
   ;;; Database
   #:connect-database
   #:disconnect-database
   #:with-database
   #:*database-type*
   #:*database-connection-spec*
   ;;; Toplevel
   #:start-server
   #:stop-server
   #:toplevel
   ;;; Provider
   #:provider
   #:find-provider
   #:configure
   ;;; Resource
   #:resource
   #:computing-resource
   #:stateful-resource
   #:description
   #:explanation
   #:pathname
   #:identification
   #:resource-error
   #:resource-confirmation
   #:actually-create-resource
   #:actually-read-resource
   #:actually-update-resource
   #:actually-delete-resource
   #:actually-import-resource
   #:actually-stop-resource
   #:actually-start-resource
   #:actually-dump-resource
   #:actually-restore-resource
   #:create-resource
   #:read-resource
   #:update-resource
   #:delete-resource
   #:import-resource
   #:stop-resource
   #:start-resource
   #:dump-resource   
   #:restore-resource   
   ;;; Null Provider
   #:null-provider
   #:make-null-provider
   ;; Docker Engine Provider
   #:docker-engine
   #:make-docker-engine
   ;;; Trac
   #:trac-ensure-valid-installation
   #:trac-list-environments
   #:trac-find-environment
   #:trac-create-environment
   #:trac-delete-environment
   #:trac-list-users
   #:trac-create-user
   #:trac-delete-user
   #:trac-add-ssh-authorized-key
   #:trac-create-git-repository
   #:trac-delete-git-repository
   #:trac-dump
   #:trac-restore
   ;;; GoCD
   #:gocd-dump
   #:gocd-restore
   ))

;;;; End of file `package.lisp'
