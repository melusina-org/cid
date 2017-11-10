;;;; cid.lisp -- Load El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid
;;;;
;;;; Copyright © 2017 Michaël Le Barbier
;;;;
;;;; This file must be used under the terms of the MIT license.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at
;;;; https://opensource.org/licenses/MIT

(defpackage :cid
  (:use :cl)
  (:export #:*base-directory* #:bootstrap))

(in-package :cid)

(defvar *base-directory*
  (if #.*load-pathname*
      (make-pathname :name nil :type nil :defaults #.*load-pathname*)
      *default-pathname-defaults*))

(defun bootstrap ()
  "Load environment"
  (ql:quickload :cl-base64))

;;; End of file `cid.lisp'
