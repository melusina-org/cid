;;;; entrypoint.lisp — Entrypoint for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(unix-opts:define-opts
  (:name :environment
   :description "The environment we are running in."
   :short #\E
   :long "environment"
   :arg-parser #'identity
   :meta-arg "ENVIRONMENT"))

(defun toplevel (&optional argv)
  "The toplevel form for the El Cid program."
  (labels
      ((usage (&optional (exit-code 0))
	 (format t "Usage: cid~% Operate an El Cid deployment.~%")
	 (uiop:quit exit-code))
       (idle-loop ()
	 (loop do (sleep 300)))
       (run-server ()
	 (restart-case
	     (progn
	       (start-server)
	       (idle-loop))
	   (abort ()
	     :report
	     (lambda (stream) (write-string "Abort system operation." stream))
	     (uiop:quit))))
       (entrypoint (&optional argv)
	 (multiple-value-bind (options free-args)
	     (if argv
		 (unix-opts:get-opts argv)
		 (unix-opts:get-opts))
	   (declare (ignore options))
	   (cond
	     ((= 0 (length free-args))
	      (run-server))
	     (t
	      (usage 64))))))
    (entrypoint argv)))

;;;; End of file `entrypoint.lisp'
