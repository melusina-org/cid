;;;; slime-filename-translation.el – Filename translation to use with Slime

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

(defun string-replace-prefix (old-prefix new-prefix string)
  (if (string-prefix-p old-prefix string)
      (concat new-prefix
	      (string-remove-prefix old-prefix string))
      string))

(defun slime-filename-translation-local-to-remote (local-name)
  (string-replace-prefix
   "/Users/michael/Melusina/cid"
   "/opt/cid/var/src/cid"
   (string-replace-prefix
    "/Users/michael/share/lisp/quicklisp"
    "/opt/cid/src/quicklisp"
    local-name)))

(defun slime-filename-translation-remote-to-local (remote-name)
  (string-replace-prefix
   "/opt/cid/var/src/cid"
   "/Users/michael/Melusina/cid"
   (string-replace-prefix
    "/opt/cid/var/quicklisp"
    "/Users/michael/share/lisp/quicklisp"
    remote-name)))

(setf slime-filename-translations nil)

(push (list "^.*$"
	    'slime-filename-translation-local-to-remote
	    'slime-filename-translation-remote-to-local)
      slime-filename-translations)

;;; End of file `slime-filename-translation.el'
