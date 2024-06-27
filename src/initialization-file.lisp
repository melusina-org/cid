;;;; iniitalization-file.lisp — Initialization Files

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

(defun read-initialization-file (designator)
  "Read initialization file from DESIGNATOR.
The designator can be a pathname or a stream."
  (declare (optimize (debug 3)))
  (labels (((setf configuration) (new-value object section key)
	     (cond ((and (null section) (assoc key object :test #'string=))
		    (rplacd (assoc key object :test #'string=) new-value))
		   ((null section)
		    (setf (cdr (last object)) (list (cons key new-value))))
		   ((assoc (first section) object :test #'string=)
		    (setf (configuration
			   (cdr (assoc (first section) object :test #'string=))
			   (rest section)
			   key)
			  new-value))
		   (t
		    (setf (cdr (last object))
			  (loop :with result = (list (cons key new-value))
				:for item :in (reverse section)
				:do (setf result
					  (list (cons item result)))
				:finally (return result)))))
	     (values new-value))
	   (section-char-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)))
	   (assoc-char-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)
		 (member char '(#\_ #\- #\Space))))
	   (empty-line-p (line)
	     (string= "" line))
	   (comment-line-p (line)
	     (and (> (length line) 0)
		  (member (char line 0) '(#\; #\#))))
	   (section-line-p (line)
	     (and (> (length line) 0)
		  (char= #\[ (char line 0))))
	   (assoc-line-p (line)
	     (and (position #\= line)
		  (every
		   #'assoc-char-p
		   (subseq line 0 (1- (position #\= line))))))
	   (parse-section (line)
	     (list* :section
		    (loop :with words = nil
			  :with start = (1+ (position #\[ line))
			  :with end = (position #\] line :from-end t)
			  :while (< start end)
			  :do (cond
				((char= #\Space (char line start))
				 (incf start))
				((char= #\. (char line start))
				 (incf start))
				((char= #\" (char line start))
				 (let ((string-start
					 (1+ start))
				       (string-end
					 (position #\" line :start (1+ start))))
				   (push (subseq line string-start string-end) words)
				   (setf start (+ 1 string-end))))
				(t
				 (let ((string-start
					 start)
				       (string-end
					 (or
					  (position-if-not #'section-char-p line :start (1+ start) :end end)
					  end)))
				   (push (subseq line string-start string-end) words)
				   (setf start (+ 1 string-end)))))
			  :finally (return (nreverse words)))))
	   (parse-assoc (line)
	     (let ((split-index
		     (position #\= line)))
	       (list
		:assoc
		(string-trim '(#\Space) (subseq line 0 (1- split-index)))
		(string-trim '(#\Space #\") (subseq line (1+ split-index))))))
	   (read-initialization-line (stream &optional continuation)
	     (let ((line
		     (read-line stream nil nil)))
	       (when line
		 (setf line (string-trim " " line)))
	       (when continuation
		 (setf line (concatenate 'string continuation line)))
	       (cond ((eq nil line)
		      (return-from read-initialization-line nil))
		     ((or (empty-line-p line)
			  (comment-line-p line))
		      (read-initialization-line stream))
		     ((char= #\\ (char line (1- (length line))))
		      (read-initialization-line
		       stream
		       (subseq line 0 (1- (length line)))))
		     ((section-line-p line)
		      (parse-section line))
		     ((assoc-line-p line)
		      (parse-assoc line))
		     (t (error "Initialization file syntax error: ~S" line)))))
	   (read-initialization-stream (stream)
	     (loop :with configuration = (cons nil nil)
		   :with section = nil
		   :for parsed-line = (read-initialization-line stream)
		   :while parsed-line
		   :do (ecase (first parsed-line)
			 (:section
			  (setf section (rest parsed-line)))
			 (:assoc
			  (setf (configuration configuration
					       section
					       (second parsed-line))
				(third parsed-line))))
		   :finally (return (cdr configuration)))))
    (etypecase designator
      (stream
       (read-initialization-stream designator))
      (pathname
       (with-open-file (stream designator
			       :direction :input
			       :if-does-not-exist :error)
	 (read-initialization-stream stream))))))

(defun write-initialization-file (configuration &optional (stream *standard-output*))
  "Write CONFIGURATION initialization file to STREAM."
  (labels ((section-char-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)))
	   (write-section (section)
	     (write-char #\[ stream)
	     (loop :for cursor :on section
		   :for item = (first cursor)
		   :for last-p = (not (cdr cursor))
		   :do (cond
			 ((every #'section-char-p item)
			  (format stream "~A" item))
			 (t
			  (format stream "~S" item)))
		   :unless last-p
		   :do (format stream "."))
	     (write-char #\] stream)
	     (write-char #\Newline stream))
	   (write-assoc (assoc)
	     (format stream "~A = ~A~%" (car assoc) (cdr assoc)))
	   (write-cell (prefix cell)
	     (let ((assocs
		     (remove-if-not #'stringp (cdr cell) :key #'cdr))
		   (subsections
		     (remove-if #'stringp (cdr cell) :key #'cdr))
		   (section
		     (append prefix (list (car cell)))))
	       (write-section section)
	       (dolist (assoc assocs)
		 (write-assoc assoc))
	       (dolist (subsection subsections)
		 (write-cell section subsection)))))
    (dolist (cell configuration)
      (write-cell nil cell))))

(defun write-initialization-file-to-string (configuration)
  "Readably write initialization file CONFIGURATION to a string."
  (with-output-to-string (stream)
    (write-initialization-file configuration stream)
    (terpri stream)))

(defun read-initialization-file-from-string (string &key (start 0) end)
  "Read an initialization file from STRING."
  (with-input-from-string (stream string :start start :end end)
    (read-initialization-file stream)))

;;;; End of file `initialization-file.lisp'
