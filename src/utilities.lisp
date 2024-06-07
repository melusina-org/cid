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

(defparameter *encryption-key* nil
  "The key used to encrypt confidential data.
It must be an octet array of length 32.")

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
  (:documentation "The slot specifications to use when readably printing OBJECT.
The slot specification is a list of slot specification. Each slot specification
is a plist with the following mandatory members

  :INITARG
    The initarg keyword used by the constructor to construct the instance.

  :SLOT-NAME
    The name of the corresponding object slot.

The following members are optional:

  :CONFIDENTIAL
    A boolean marking slots which requires encryption when they are persisted.
    Only string values which can be marked as CONFIDENTIAL.

  :PRESENTATION
    When provided, a function used to transform the slot value in a value
    which can be persisted.

  :EXTERNAL
    This flag indicates a slot-value which is determined by external factors.
    It is forbidden for the user to modify this value.

  :IMMUTABLE
    This flag indicates a slot-value which cannot be modified by the Steward
    of a resource.  It means that modifying that slot of the resource is only
    possible by deleting and recreating the resource."))

(defun write-persistent-object (object stream)
  "Readably write persistent OBJECT on STREAM."
  (let ((class
	  (type-of object))
	(slot-specs
	  (persistent-slots object)))
    (labels ((pprint-readable-property (name value stream)
	       (pprint-logical-block (stream nil)
		 (pprint name stream)
		 (write-char #\Space stream)
		 (pprint-newline :linear stream)
		 (pprint value stream)))
	     (pprint-readable-slot-value (object stream &key slot-name &allow-other-keys)
	       (pprint (slot-value object slot-name) stream))
	     (pprint-presented-slot-value (object stream &key slot-name presentation &allow-other-keys)
	       (pprint (funcall presentation (slot-value object slot-name)) stream))
	     (pprint-confidential-string (object stream &key slot-name &allow-other-keys)
	       (pprint-logical-block (stream nil :prefix "[" :suffix "]")
	       (pprint 'confidential-string stream)
	       (write-char #\Space stream)
	       (let* ((initialization-vector
			(ironclad:random-data 16))
		      (cipher
			(ironclad:make-cipher :aes
					      :mode :ofb
					      :key *encryption-key*
					      :initialization-vector
					      initialization-vector))
		      (plaintext
			(flexi-streams:string-to-octets
			 (slot-value object slot-name)
			 :external-format :utf-8))
		      (ciphertext
			(make-array (length plaintext)
				    :element-type '(unsigned-byte 8)
				    :initial-element 0)))
		 (ironclad:encrypt cipher plaintext ciphertext)
		 (pprint-readable-property
		  :initialization-vector
		  (ironclad:byte-array-to-hex-string initialization-vector)
		  stream)
		 (write-char #\Space stream)
		 (pprint-newline :linear stream)
		 (pprint-readable-property
		  :ciphertext
		  (ironclad:byte-array-to-hex-string ciphertext)
		  stream)))))
      (pprint-logical-block (stream nil :prefix "[" :suffix "]")
	(pprint class stream)
	(write-char #\Space stream)
	(pprint-newline :linear stream)
	(pprint-indent :current 0 stream)
	(loop :for slot-spec-iterator :on slot-specs
	      :for slot-spec = (first slot-spec-iterator)
	      :for lastp = (not (rest slot-spec-iterator))
	      :do (destructuring-bind (&key initarg slot-name confidential immutable presentation) slot-spec
		    (declare (ignore immutable))
		    (when (slot-boundp object slot-name)
		      (pprint-logical-block (stream nil)
			(pprint initarg stream)
			(write-char #\Space stream)
			(pprint-newline :linear stream)
			(cond
			  ((and confidential *encryption-key*)
			   (apply #'pprint-confidential-string object stream slot-spec))
			  (confidential
			   (error "The slot ~A is marked as confidential and no encryption key is defined." slot-name))
			  (presentation
			   (apply #'pprint-presented-slot-value object stream slot-spec))
			   
			  (t
			   (apply #'pprint-readable-slot-value object stream slot-spec))))
		      (unless lastp
			(write-char #\Space stream)
			(pprint-newline :linear stream)))))))))

(defun write-persistent-object-to-string (object)
  "Readably write persistent OBJECT to a string."
  (with-output-to-string (stream)
    (let ((*print-readably* t)
	  (*print-circle* t)
          (*package* (find-package :keyword)))
      (pprint object stream)
      (terpri stream))))

(defun save-persistent-object (object version-name)
  "Persist OBJECT."
  (destructuring-bind (tenant-name project-name object-name)
      (list (name (tenant object)) (name (project object)) (name object))
    (let ((filename
	    (user-data-relative-pathname
	     tenant-name
	     project-name
	     (concatenate 'string object-name ".lisp"))))
      (ensure-directories-exist filename) 
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
	(format stream "~A~%" version-name)
	(let ((*print-readably* t)
              (*print-circle* t)
              (*package* (find-package :keyword)))
	  (write-persistent-object object stream)
	  (terpri stream))
	(finish-output stream))
      (values object filename version-name))))

(defun read-persistent-object (stream)
  "Read a persistent object from STREAM."
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

(defun read-persistent-object-from-string (string &key (start 0) end)
  "Read a persistent object from STRING."
  (with-input-from-string (stream string :start start :end end)
    (read-persistent-object stream)))

(defun load-persistent-object (object allowed-version-names)
  "Read persisted object from corresponding file.
The OBJECT must either be a list consisting of three strings
TENANT-NAME, PROJECT-NAME or OBJECT-NAME or an object with a
TENANT, a PROJECT and a NAME."
  (destructuring-bind (tenant-name project-name object-name)
      (cond ((and (listp object) (= 3 (length object)) (every #'stringp object))
	     object)
	    (t
	     (list (name (tenant object)) (name (project object)) (name object))))
    (let ((filename
	    (user-data-relative-pathname
	     tenant-name project-name
	     (concatenate 'string object-name ".lisp"))))
      (assert (probe-file filename) () 'file-does-not-exist)
      (with-open-file (stream filename :direction :input)
	(let ((version (read-line stream)))
	  (assert (member version (alexandria:ensure-list allowed-version-names) :test #'string=)
		  () 'file-version-is-not-allowed)
          (let ((persistent-object
		  (read-persistent-object stream)))
	  (values persistent-object filename version)))))))

(defmethod persistent-constructor ((class (eql 'confidential-string)))
  'decrypt-confidential-string)

(defun decrypt-confidential-string (&key initialization-vector ciphertext)
  (let ((cipher
	  (ironclad:make-cipher :aes
				:mode :ofb
				:key *encryption-key*
				:initialization-vector
				(ironclad:hex-string-to-byte-array initialization-vector)))
	(plaintext
	  (make-array (/ (length ciphertext) 2)
		      :element-type '(unsigned-byte 8)
		      :initial-element 0)))
    (ironclad:decrypt cipher (ironclad:hex-string-to-byte-array ciphertext) plaintext)
    (flexi-streams:octets-to-string plaintext :external-format :utf-8)))


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
