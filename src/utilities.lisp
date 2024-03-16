;;;; utilities.lisp — Utilities for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defun user-data-relative-pathname (&rest more)
  "Prepare a PATHNAME relative to the user data of the application."
  (apply #'uiop:xdg-data-home
	 #.(string-downcase (package-name *package*)) more))


;;;;
;;;; Property Lists
;;;;

(defun sort-plist (plist)
  "Sort the provided PLIST so that its keys are in ascending order."
  (flet ((remove-undefined-properties (alist)
	   (remove nil alist :key #'cdr))
	 (sort-properties (alist)
	   (sort alist #'string< :key #'car)))    
  (alexandria:alist-plist
   (sort-properties
    (remove-undefined-properties
     (alexandria:plist-alist plist))))))

(defun remove-property (plist property)
  "Return a copy from PLIST with PROPERTY removed."
  (loop :for (name value . tail) :on plist
	:for property-p = t :then (not property-p)
	:when (and property-p (not (eq property name)))
	:append (list name value)))


;;;;
;;;; Check instance slots
;;;;

(defun check-that-instance-slot-matches-value (instance slot-name value)
  (flet ((matches-p (value-1 value-2)
	   (typecase value-1
	     (string
	      (string= value-1 value-2))
	     (t
	      (equalp value-1 value-2)))))
    (unless (matches-p value (slot-value instance slot-name))
      (error "The slot ~A of the deserialised instance does not match the initargs." slot-name))))

(defmacro check-that-instance-slots-match-initargs (instance initargs)
  (flet ((initarg-symbol (initarg)
	   (cond
	     ((symbolp initarg)
	      initarg)
	     ((listp initarg)
	      (first initarg))))
	 (initarg-slot-name (initarg)
	   (cond
	     ((symbolp initarg)
	      (list 'quote initarg))
	     ((listp initarg)
	      (list 'quote (second initarg))))))
    (alexandria:once-only (instance)
      (let ((body
	      (loop :for initarg :in initargs
		    :collect
		    `(check-that-instance-slot-matches-value
		      ,instance
		      ,(initarg-slot-name initarg)
		      ,(initarg-symbol initarg)))))
	`(progn ,@body)))))


;;;;
;;;; File Checksum
;;;;

(defun file-checksum (pathname &optional (digest-name :sha256))
  "Compute a checksum of PATHNAME using DIGEST-NAME."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file digest-name pathname)))


;;;;
;;;; Portable Filename Character Set
;;;;

(defun portable-filename-character-set-p (string)
  "Predicate that recognises a STRING in the portable filename character set."
  (labels ((portable-char-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)
		 (position char "-_."))))
    (every #'portable-char-p string)))
 
(defun check-that-string-is-in-the-portable-filename-character-set (string)
  "Check that STRING is in the portable filename character set."
  (unless (portable-filename-character-set-p string)
    (error "The string ~A does not consist of characters form the portable set." string)))

;;;; End of file `utilities.lisp'
