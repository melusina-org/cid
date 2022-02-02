;;;; utilities.lisp — Utilities for El Cid

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

(in-package #:org.melusina.cid)

(defun bsd-install-directory (directory &key owner group mode)
  "Create DIRECTORY whith the given OWNER, GROUP and MODE.
If a directory with the given owner, group and mode already
exists, the command exits succesfully.

DIRECTORY can also be a list of directories."
  (rashell:run-utility
   (make-instance 'rashell:command
		  :program #p"/usr/bin/install"
		  :argv (concatenate 'list
				     (list "-d" "-o" owner "-g" group "-m" (write-to-string mode :base 8))
				     (mapcar #'namestring (ensure-list directory))))))


(defun bsd-install-empty-file (file &key owner group mode)
  "Create empty FILE whith the given OWNER, GROUP and MODE."
    (rashell:run-utility
     (make-instance 'rashell:command
		    :program #p"/usr/bin/install"
		    :argv (list "-o" owner "-g" group "-m" (write-to-string mode :base 8)
				"/dev/null" (namestring file)))))

;;;; End of file `utilities.lisp'
