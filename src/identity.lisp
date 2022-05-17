;;;; identity.lisp — Identity for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class user (tenant-scope)
  ((userid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :userid)
   (pathname
    :accessor user-pathname
    :type string
    :initarg :pathname)
   (displayname
    :accessor user-displayname
    :type string
    :initarg :displayname)
   (role
    :accessor user-role
    :type keyword
    :initarg :role
    :description "Should be one of :USER, :ADMINISTRATOR, :WITNESS."))
  (:base-table user))

(defun user-tenant (user)
  "The tenant of USER."
  (slot-value user 'tenant))

(defun find-user (designator tenant)
  "Find the user associated to DESIGNATOR in TENANT."
  (ensure-tenant-scope (tenant)
    (typecase designator
      (user
       (and (eq tenantid (slot-value designator 'tenantid))
	    designator))
      (string
       (caar (clsql:select 'user :where [and [= [slot-value 'user 'pathname] designator]
                                             [= [slot-value 'user 'tenantid] tenantid]])))
      (integer
       (caar (clsql:select 'user :where [= [slot-value 'user 'userid] designator]))))))

(defun make-user (&key pathname displayname role tenant)
  "Make a USER with the given attributes.
When PATHNAME designates an already existing user and its other attributes match,
then this user is returned.  When PATHNAME designates an already existing user otherwise,
then an error condition is signaled."
  (labels ((other-attributes-match-p (user)
	     (unless user
	       (return-from other-attributes-match-p user))
	     (unless (string-equal displayname (slot-value user 'displayname))
	       (error "make-user: ~A (~A): A user with this pathname exists but some attribute differs.
The attribute DISPLAYNAME in the existing user is ~S but should be ~S."
		      pathname (slot-value tenant 'pathname)
		      (slot-value user 'displayname) displayname))
	     (unless (equal role (slot-value user 'role))
	       (error "make-user: ~A (~A): A user with this pathname exists but some attribute differs.
The attribute ROLE in the existing user is ~S but should be ~S."
		      pathname (slot-value tenant 'pathname)
		      (slot-value user 'role) role))
	     user))
    (let* ((tenant
	     (find-tenant tenant))
	   (user
	     (or (other-attributes-match-p
		  (find-user pathname tenant))
		 (make-instance 'user
				:tenantid (slot-value tenant 'tenantid)
				:pathname pathname
				:displayname displayname
				:role role))))
      (clsql:update-records-from-instance user)
      user)))

;;;; End of file `identity.lisp'
