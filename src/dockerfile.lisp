;;;; dockerfile.lisp -- Generate dockerfiles

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid
;;;;
;;;; Copyright © 2017 Michael Le Barbier Grünewald
;;;;
;;;; This file must be used under the terms of the MIT license.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at
;;;; https://opensource.org/licenses/MIT

(defpackage :cid.dockerfile
  (:use :cl :sb-gray)
  (:export #:file-from-file #:stream-from-stream #:string-from-string)
  (:documentation
   "Generate dockerfile.

Function to generate dockerfiles that can take advantage of the docker
cache in an efficient way."))

(in-package :cid.dockerfile)


;;
;; Output & Format
;;

(defparameter *dockerfile-output* t
  "The output for Dockerfile generation.")

(defun dockerfile-format (fmt &rest args)
  (apply #'format *dockerfile-output* fmt args))


;;
;; Dockerfile parameters
;;

(defparameter *dockerfile-parameters* nil)

(defun set-parameter (key value)
  (delete key *dockerfile-parameters* :key #'first :test #'string=)
  (push (list key value) *dockerfile-parameters*))

(defun get-parameter (key &optional default)
  (if (assoc key *dockerfile-parameters*)
      (second (assoc key *dockerfile-parameters*))
      default))

(defmacro with-parameters (params &body body)
  `(let ((*dockerfile-parameters* ,params))
     ,@body))

;;
;; Read file
;;

(defparameter *dockerfile-directives*
  '(add cmd copy entrypoint env expose from parameter run script user volume)
  "The list of directives allowed in dockerfiles.")

(defun do-tokens (str func &optional (ax0 nil))
  (do ((token (read-token str) (read-token str))
       (ax ax0 (funcall func token ax)))
      ((eql token nil) (funcall func token ax))))

(defun %peek-char (str)
  (peek-char nil str nil nil))

(defun skip-whitespaces (str)
  "Skip all whitespace characters found at the current position of STR."
  (do nil
      ((not (position (%peek-char str) '(#\space #\tab))) nil)
    (read-char str)))

(defun read-until (str predicate &optional (junk t) (interpret-escape-sequences t))
  "Read characters from STR until the first character matching PREDICATE.

The result is the string made of the characters that has been read,
excluding the character that satisfied PREDICATE.

If JUNK is T, the character satisfying PREDICATE is removed. If it is
NIL, that character remains available for reading at the beginning of
STR.

If INTERPRET-ESCAPE-SEQUENCES is T, a backslash character prevent the
character immediately following it to be examined by predicate. The
backslash itself is not kept in the result."
  (with-output-to-string (ax)
    (do ((ch (%peek-char str) (%peek-char str) ))
        ((or (eq ch nil) (funcall predicate ch))
         (when (and (eq ch nil) (not (funcall predicate ch)))
           (error "Unexpected EOF"))
         (when (and ch junk)
           (read-char str)))
      (if (and interpret-escape-sequences (eq ch #\backslash))
          (progn
            (read-char str)
            (if (%peek-char str)
                (write-char (read-char str) ax)
                (error "Unexpected EOF while reading escape sequence")))
          (progn
            (read-char str)
            (write-char ch ax))))))

(defun read-token (str)
  "Read a token from STR."
  (labels ((read-token-entrypoint (str)
             (case (%peek-char str)
               (#\backslash
                (read-token-escaped str))
               ((#\space #\tab)
                (skip-whitespaces str)
                '(whitespace))
               (#\newline
                (read-char str)
                '(newline))
               (#\left_square_bracket
                (read-char str)
                '(list-begin))
               (#\right_square_bracket
                (read-char str)
                '(list-end))
               (#\comma
                (read-char str)
                '(list-sep))
               ((#\semicolon #\number_sign)
                (list 'comment (read-comment str)))
               (#\quotation_mark
                (list 'interpreted-string (read-interpreted-string str)))
               (#\apostrophe
                (list 'literal-string (read-literal-string str)))
               ((nil) nil)
               (t
                (read-token-word str))))
           (read-token-escaped (str)
             (read-char str)
             (case (%peek-char str)
               (#\newline
                (read-char str)
                (skip-whitespaces str)
                '(whitespace))
               ((nil)
                (error "Unexpected EOF while reading escape sequence"))
               (t
                (read-token-word str))))
           (read-token-word (str)
             (list 'word (read-word str))))
    (let* ((token-start (stream-file-position str))
           (token (read-token-entrypoint str))
           (token-end (stream-file-position str)))
      (when token (concatenate 'list token (list token-start token-end))))))

(defun read-comment (str)
  (read-until str (lambda (ch) (position ch '(#\newline nil))) t nil))

(defun read-literal-string (str)
  (read-char str)
  (read-until str (lambda (ch) (eq ch #\apostrophe)) t nil))

(defun read-interpreted-string (str)
  (read-char str)
  (read-until str (lambda (ch) (eq ch #\quotation_mark)) t t))

(defun read-word (str)
  (read-until str (lambda (ch) (position ch '(nil #\space #\tab #\newline #\backslash))) nil nil))

(defun parse (read-token)
  (labels ((parse-eof (file)
             (reverse file))
           (parse-line (file ax)
             (let ((token (funcall read-token)))
               (case (when token (first token))
                 ((nil)
                  (parse-eof (if ax (cons (reverse ax) file) file)))
                 ((comment whitespace)
                  (parse-line file ax))
                 (newline
                  (parse-line (if ax (cons (reverse ax) file) file) nil))
                 (word
                  (let* ((word (second token))
                         (directive
                           (find-if (lambda (directive)
                                      (string= (symbol-name directive) word))
                                    *dockerfile-directives*)))
                    (if (and (null ax) directive)
                        (case directive
                          (parameter
                           (parse-parameter-key file nil))
                          (t
                           (parse-line file (list directive))))
                        (parse-line file (cons token ax)))))
                 ((literal-string interpreted-string)
                  (parse-line file (cons token ax)))
                 (list-begin
                  (parse-list-item file ax nil))
                 (t
                  (error "Unexpected token ~S while parsing line" (first token))))))
           (parse-parameter-key (file ax)
             (let ((token (funcall read-token)))
               (case (when token (first token))
                 (word
                  (parse-parameter-value file ax (second token)))
                 (newline
                  (parse-line (cons (list 'parameter (reverse ax)) file) nil))
                 (whitespace
                  (parse-parameter-key file ax))
                 (t
                  (error "Unexpected token ~S while parsing parameter key" (first token))))))
           (parse-parameter-value (file ax key)
             (let ((token (funcall read-token)))
               (case (when token (first token))
                 ((nil)
                  (error "Unexpected EOF while parsing parameter value"))
                 ((word literal-string interpreted-string)
                  (parse-parameter-key file (cons (list key token) ax)))
                 (whitespace
                  (parse-parameter-value file ax key))
                 (t
                  (error "Unexpected token ~S while parsing parameter value" (first token))))))
           (parse-list-item (file line ax)
             (let ((token (funcall read-token)))
               (case (when token (first token))
                 ((nil)
                  (error "Unexpected EOF while parsing list"))
                 ((comment newline list-begin list-sep)
                  (error "Unexpected ~S while parsing list" (first token)))
                 (whitespace
                  (parse-list-item file line ax))
                 (t
                  (parse-list-next file line (cons token ax))))))
           (parse-list-next (file line ax)
             (let ((token (funcall read-token)))
               (case (when token (first token))
                 ((nil)
                  (error "Unexpected EOF while parsing list"))
                 (whitespace
                  (parse-list-next file line ax))
                 (list-sep
                  (parse-list-item file line ax))
                 (list-end
                  (parse-line file (cons (cons 'list (reverse ax)) line)))
                 (t
                  (error "Unexpected ~S while parsing list" (first token)))))))
    (parse-line nil nil)))

;;
;; Generate
;;

(defun generate-script (out script)
  "Generate final script deduced from SCRIPT and emit it on OUT."
  (let ((*dockerfile-parameters* nil))
    (labels
        ((format-token (str token)
           (case (first token)
             (word
              (format str "~A" (second token)))
             (literal-string
              (format str "'~A'" (second token)))
             (interpreted-string
              (format str "\"~A\"" (second token)))
             (list
              (format str "[")
              (unless (null (rest token))
                (format-token str (first (rest token))))
              (dolist (item (rest (rest token)))
                (format str ",")
                (format-token str item))
              (format str "]"))
             (t
              (error "Cannot write ~S token" (first token)))))
         (format-parameters (str parameters)
           (format str "### Dockerfile Parameters~2%")
           (dolist (binding parameters)
             (format str "~A=" (first binding))
             (format-token str (second binding))
             (format str "~%"))
           (format str "~%"))
         (string-from-string-token (token)
           (case (first token)
             ((word literal-string interpreted-string)
              (format nil "~A" (second token)))
             (t
              (error "Token ~S is not a string token" (first token)))))
         (multiple-values-script-args (first &optional second &rest args)
           (flet ((user-name-p (text)
                    (not
                     (position-if (complement (lambda (ch)
                                                (or (alphanumericp ch)
                                                    (position ch "-"))))
                                  text))))
             (case (first first)
               (word
                (if (user-name-p (string-from-string-token first))
                    (progn
                      (unless second
                        (error "This SCRIPT statement does not refer to any file"))
                      (values
                       (string-from-string-token first)
                       (string-from-string-token second)
                       args))
                    (values
                     nil
                     (string-from-string-token first)
                     (if second
                         (list* second args)
                         args))))
               ((literal-string interpreted-string)
                (values
                 nil
                 (string-from-string-token first)
                 (list* second args)))
               (t
                (error "Unexpected token ~S when in SCRIPT statement" (first first))))))
         (format-script (str user path &rest args)
           (let ((text
                   (with-output-to-string (buffer)
                     (format-parameters buffer *dockerfile-parameters*)
                     (with-open-file (handle path)
                       (do ((ch (read-char handle nil 'eof)
                                (read-char handle nil 'eof)))
                           ((eql ch 'eof))
                         (write-char ch buffer)))
                     (when args
                       (format buffer "~%")
                       (format-token buffer (first args))
                       (dolist (token (rest args))
                         (format buffer " ")
                         (format-token buffer token))))))
             (format str "RUN echo '")
             (format str (base64:string-to-base64-string text))
             (format str "' | base64 --decode | su -")
             (when user
               (format str " ~A" user))
             (format str "~%"))))
      (dolist (stmt script)
        (case (first stmt)
          ((add cmd copy entrypoint env from run volume)
           (format out (string-upcase (symbol-name (first stmt))))
           (dolist (token (rest stmt))
             (format out " ")
             (format-token out token))
           (format out "~%"))
          (parameter
           (dolist (binding (second stmt))
             (set-parameter (first binding) (second binding))))
          (script
           (multiple-value-bind (user path args)
               (apply #'multiple-values-script-args (rest stmt))
             (apply #'format-script out user path args)))
          (t
           (error "Cannot translate ~S~%" stmt)))))))

;;
;; Interface
;;

(defun string-from-string (here-document)
  "Generate the content of a dockerfile from a given HERE-DOCUMENT."
  (with-output-to-string (out)
    (with-input-from-string (str here-dodument)
      (generate-script out (parse (lambda () (read-token str)))))))

(defun stream-from-stream (&optional (input *standard-input*) (output *standard-output*))
  (generate-script output (parse (lambda () (read-token input)))))

(defun file-from-file (path-input path-output)
  (with-open-file (output path-output :direction :output :if-exists :supersede)
    (with-open-file (input path-input :direction :input)
      (generate-script (output (parse (lambda () (read-token input))))))))

(defun test (path-input)
  (with-open-file (input path-input :direction :input)
    (stream-from-stream input)))

;;;; End of file `dockerfile.lisp'
