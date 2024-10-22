;;;; entry-point.lisp — Entrypoint for El Cid Administration Console

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/console)

(defun idle-loop ()
  (loop (sleep 5)))

(defun load-project ()
  (let ((pathname
	  #p"/opt/cid/var/config/project.lisp"))
    (assert (probe-file pathname) () 'file-does-not-exist)
    (let ((cid:*encryption-key*
	    (ironclad:hex-string-to-byte-array
	     (uiop:getenv "CID_ENCRYPTION_KEY"))))
      (with-open-file (stream pathname :direction :input)
	(values
	 (cid:read-persistent-object stream)
	 pathname)))))

(defun entry-point ()
  (format t "Administration Console for El Cid.~%")
  (start-swank)
  (idle-loop)
  (uiop:quit 0))

;;;; End of file `entry-point.lisp'
