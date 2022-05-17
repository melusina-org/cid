;;;; utilities.lisp — Utilities for El Cid

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


;;;;
;;;; Ramdom Strings
;;;;

(defparameter *alphabet-hexadecimal* "0123456789abcdef"
  "The set of hexadecimal characters.")

(defparameter *alphabet-base36* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The set of characters used for base 36 encoding.")

(defparameter *alphabet-base64* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz/-"
  "The set of characters used for base 64 encoding.")

(defun random-string (&optional (length 32) (alphabet *alphabet-base36*))
  "Prepare a random alphabetic string of given LENGTH.

The returned string contains LENGTH characters chosen from
the vector ALPHABET.

This uses a very weak method that does not try to avoid collisions.x"
  (loop :with id = (make-string length)
        :with alphabet-length = (length alphabet)
        :for i :below length
        :do (setf (aref id i)
                  (aref alphabet (random alphabet-length)))
        :finally (return id)))


;;;;
;;;; Property Lists
;;;;

(defun sort-plist (plist)
  "Sort the provided PLIST so that its keys are in ascending order."
  (alexandria:alist-plist
   (sort (alexandria:plist-alist plist)
	 #'string<
	 :key #'car)))


;;;;
;;;; Unix utilities
;;;;

(defun bsd-install-directory (directory &key owner group mode)
  "Create DIRECTORY whith the given OWNER, GROUP and MODE.
If a directory with the given owner, group and mode already
exists, the command exits succesfully.

DIRECTORY can also be a list of directories."
  (rashell:run-utility
   (rashell:make-command
    :program #p"/usr/bin/install"
    :argv (concatenate 'list
		       (list "-d" "-o" owner "-g" group "-m" (write-to-string mode :base 8))
		       (mapcar #'namestring (ensure-list directory))))))


(defun bsd-install-empty-file (file &key owner group mode)
  "Create empty FILE whith the given OWNER, GROUP and MODE."
    (rashell:run-utility
     (rashell:make-command
      :program #p"/usr/bin/install"
      :argv (list "-o" owner "-g" group "-m" (write-to-string mode :base 8)
		  "/dev/null" (namestring file)))))

;;;; End of file `utilities.lisp'
