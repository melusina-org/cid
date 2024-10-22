;;;; slime-filename-translation.el – Filename translation to use with Slime -*- lexical-binding:t -*-

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; Evaluate this buffer to suupport Slime/Emacs filename translations
;;;; when developing the Console Server running in a Docker container
;;;; (the remote system). Set the hostname regular expression
;;;; accordingly.
;;;;
;;;; Adjust the hostname, path to working copy and path to quicklisp
;;;; in the following example:
;;;;
;;;; (setf slime-filename-translations
;;;;       (list
;;;;        (cid-slime-filename-translations
;;;; 	"^console[.]forge[.]melusina[.]local$"
;;;; 	"~/Lisp/cid"
;;;; 	"~/share/lisp/quicklisp")))

(defun string-replace-prefix (string old-prefix new-prefix)
  (if (string-prefix-p old-prefix string)
      (concat new-prefix (string-remove-prefix old-prefix string))
      string))

(defun string-replace-prefixes (string replacement-specs)
  (let ((answer string))
    (dolist (replacement-spec replacement-specs answer)
      (setq answer (apply 'string-replace-prefix answer replacement-spec)))))

(defun cid-slime-filename-translations (hostname cid quicklisp)
  (list hostname
	#'(lambda (local-filename)
	    (string-replace-prefixes
	     local-filename
	     (list
	      (list cid
		    "/opt/cid/var/quicklisp/local-projects/cid")
	      (list quicklisp
			 "/opt/cid/var/quicklisp"))))
	(lambda #'(cid quicklisp) (remote-filename)
	  (string-replace-prefixes
	   remote-filename
	   (list
	    (list "/opt/cid/var/quicklisp/local-projects/cid"
		  cid)
	    (list "/opt/cid/var/quicklisp"
		  quicklisp))))))

;;;; End of file `slime-filename-translation.el'
