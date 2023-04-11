;;;; database.lisp — Database Connection for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defparameter *database-type* nil
  "The connection type for the database.
When the value is NIL, a sensible default is provided by the `INITIALISE-DATABASE'
function.")

(defparameter *database-connection-spec* nil
  "The connection specification for the database.
When the value is NIL, a sensible default is provided by the `INITIALISE-DATABASE'
function.")

(defparameter *database-application-class-list* '(tenant user project)
  "The list of application classes to initialise when connecting to the database.")

(defun connect-database ()
  "Connect the database.
If the database does not exist, it is created and populated."
  (unless *database-type*
    (setf *database-type* :sqlite3))
  (unless *database-connection-spec*
    (setf *database-connection-spec* '("CID")))
  (if (clsql:probe-database *database-connection-spec* :database-type *database-type*)
      (clsql:connect *database-connection-spec* :database-type *database-type*)
      (prog2 (clsql:create-database *database-connection-spec* :database-type *database-type*)
	  (clsql:connect *database-connection-spec* :database-type *database-type*)
	(dolist (application-class *database-application-class-list*)
	  (clsql:create-view-from-class application-class)))))

(defun disconnect-database ()
  "Disconnect the database."
  (clsql:disconnect))

(defmacro with-database (&body body)
  "Run body under a connected database."
  `(unwind-protect
	(progn (connect-database) ,@body)
     (disconnect-database)))

;;;; End of file `database.lisp'
