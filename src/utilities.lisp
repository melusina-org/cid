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

(defun plist-p (object &key test test-not)
  "Predicate recognising property lists."
  (when (and test test-not)
    (error "The TEST and TEST-NOT arguments cannot be simultaneously provided."))
  (flet ((test-value (value)
	   (and (or (not test) (funcall test value))
		(or (not test-not) (not (funcall test-not value))))))
    (and (alexandria:proper-list-p object)
	 (evenp (length object))
	 (loop :for (name value . tail) :on object :by #'cddr
	       :always (and (keywordp name) (test-value value))))))


;;;;
;;;; Association Lists
;;;;

(defun alist-p (object)
  "Predicate recognising association lists."
  (and (alexandria:proper-list-p object)
       (every #'consp object)))


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
;;;; Write and read persistent Object
;;;;

(defgeneric persistent-constructor (class)
  (:documentation "The constructor symbol to use when readably printing OBJECT."))

(defgeneric persistent-slots (object)
  (:method-combination append)
  (:documentation "The slot specifications to use when readably printing OBJECT."))

(defun write-persistent-object (object stream &optional constructor slot-specs)
  "Readably print OBJECT on STREAM."
  (let ((constructor
	  (or constructor (type-of object)))
	(slot-specs
	  (or slot-specs (persistent-slots object))))
    (pprint-logical-block (stream nil :prefix "[" :suffix "]")
      (pprint constructor stream)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (pprint-indent :current 0 stream)
      (loop :for slot-spec-iterator :on slot-specs
	    :for slot-spec = (first slot-spec-iterator)
	    :for lastp = (not (rest slot-spec-iterator))
	    :do (destructuring-bind (initarg slot-name &key sensitive-p (presentation 'identity)) slot-spec
		  (declare (ignore sensitive-p))
		  (when (slot-boundp object slot-name)
		    (pprint-logical-block (stream nil)
		      (pprint initarg stream)
		      (write-char #\Space stream)
		      (pprint-newline :linear stream)
		      (pprint (funcall presentation (slot-value object slot-name))
			      stream))
		    (unless lastp
		      (write-char #\Space stream)
		      (pprint-newline :linear stream))))))))

(defun read-persistent-object (stream)
  "Read an object from STREAM."
  (flet ((read-persisted-object (stream char)
	   (declare (ignore char))
	   (let* ((delimited-list
		    (read-delimited-list #\] stream t))
		  (class
		    (first delimited-list))
		  (initargs
		    (rest delimited-list)))
	     (apply (persistent-constructor class) initargs))))
    (let ((readable-readtable (copy-readtable)))
      (set-macro-character #\[ #'read-persisted-object nil readable-readtable)
      (set-syntax-from-char #\] #\) readable-readtable)
      (let ((*readtable* readable-readtable)
	    (*read-eval* nil))
	(read stream)))))


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


;;;;
;;;; Extract JSON Fields
;;;;

(defun extract-json-fields (string fields)
  "Extract FIELDS from TEXT and return them in a property list.
The items of the FIELDS specification describe how to extract fields
from the text.  Each item in the FIELDS specification is a property
list with the following entries:

  :NAME STRING
    The NAME of the object field where the value is stored.
  :TYPE SYMBOL
    The TYPE of the value. This is one of
      'INTEGER, 'STRING, '(OR STRING NULL), '(LIST STRING)
  :PROPERTY KEYWORD
    The name of the PROPERTY where the value is to be stored.
  :KEY FUNCTION
    A function to apply on the field value.
"
  (flet ((extract-json-field (object &key name type property (key 'identity))
	   (flet ((fetch ()
		    (multiple-value-bind (value present-p) (gethash name object)
		      (unless present-p
			(error "Cannot read field ~A from JSON text." name))
		      (alexandria:switch (type :test #'equal)
			('integer
			 (check-type value integer)
			 (values value))
			('string
			 (string value))
			('(or string null)
			  (unless (string= "<none>" value)
			    (string value)))
			('(list string)
			  (loop :for item :in value
				:collect (string item)))
			(t
			 (error "Cannot extact field of type ~A from JSON text." type)))))
		  (extract (text)
		    (when text
		      (funcall key text)))
		  (pack (value)
		    (list property value)))
	     (pack (extract (fetch))))))
    (loop :with object = (yason:parse string)
	  :for field :in fields
	  :nconc (apply #'extract-json-field object field))))


;;;;
;;;; Random String
;;;;

(defun random-string (&optional (length 32) (alphabet :base36))
  "Prepare a random alphabetic string of given LENGTH.

The returned string contains LENGTH characters chosen from
the vector ALPHABET.  When ALPHABET is one of the keywords

  :HEXADECIMAL :BASE36 and :BASE64

the corresponding alphabet is used.

This uses a very weak method that does not try to avoid collisions.x"
  (flet ((actual-alphabet (alphabet)
	   (case alphabet
	     (:hexadecimal
	      "0123456789abcdef")
	     (:base36
	      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
	     (:base64
	       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz/-")
	     (t
	      alphabet))))
    (loop :with id = (make-string length)
	  :with actual-alphabet = (actual-alphabet alphabet)
          :with alphabet-length = (length actual-alphabet)
          :for i :below length
          :do (setf (aref id i)
                    (aref actual-alphabet (random alphabet-length)))
          :finally (return id))))

;;;; End of file `utilities.lisp'
