;;;; clsql-extension.lisp — Extension to CLSQL

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/clsql-extension
  (:use #:cl))

(in-package #:org.melusina.cid/clsql-extension)

(defmethod clsql-sys::database-output-sql ((instance pathname) database)
  (clsql-sys::database-output-sql (namestring instance) database))

(defmethod clsql-sys:read-sql-value (val (type (eql 'pathname)) database db-type)
  (declare (ignore database db-type))
  (pathname val))

(defmethod clsql::database-get-type-specifier ((type (eql 'pathname)) args database db-type)
  (declare (ignore args database db-type))
  "VARCHAR(255)")

;;;; End of file `clsql-extension.lisp'
