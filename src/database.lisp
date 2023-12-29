;;;; database.lisp — Database Connection for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defparameter *database-type* :sqlite3
  "The connection type for the database.")

(defparameter *database-connection-spec*
  (list (namestring (user-data-relative-pathname "database.sqlite")))
  "The connection specification for the database.")

(defparameter *database-application-class-list*
  '(tenant project)
  "The list of application classes to initialise
when connecting to the database for the first time.")

(defun connect-database ()
  "Connect the database.
If the database does not exist, it is created and populated."
  (if (clsql:probe-database *database-connection-spec*
			    :database-type *database-type*)
      (clsql:connect *database-connection-spec*
		     :database-type *database-type*)
      (prog2 (clsql:create-database *database-connection-spec*
				    :database-type *database-type*)
	  (clsql:connect *database-connection-spec*
			 :database-type *database-type*)
	(dolist (application-class *database-application-class-list*)
	  (clsql:create-view-from-class application-class)))))

(defun disconnect-database ()
  "Disconnect the database."
  (clsql:disconnect))

(defmacro with-database (&body body)
  "Run BODY with a connected database."
  `(unwind-protect
	(progn (connect-database) ,@body)
     (disconnect-database)))

;;;; End of file `database.lisp'
