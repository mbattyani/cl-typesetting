;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package typeset)

(defconstant +huge-number+ (truncate most-positive-fixnum 10))
(defconstant +epsilon+ 0.0001)

(defvar *leading-ratio* 1.2)

;;
;; FLAG -- collect all these in *default-text-style* and *current-text-style* ;; djc
;;
(defvar *default-font* (pdf:get-font))
(defvar *default-font-size* 12.0)
(defvar *default-text-x-scale* 1)
(defvar *default-color* '(0 0 0))
(defvar *default-back-color* '(1.0 1.0 1.0))
(defvar *default-h-align* :left)
(defvar *default-v-align* :top)
(defvar *default-left-margin* 0)
(defvar *default-right-margin* 0)

(defvar *font* *default-font*)
(defvar *font-size* *default-font-size*)
(defvar *text-x-scale* *default-text-x-scale*)
(defvar *color* *default-color*)
(defvar *back-color* *default-back-color*)
(defvar *h-align* *default-h-align*)
(defvar *v-align* *default-v-align*)
(defvar *left-margin* *default-left-margin*)
(defvar *right-margin* *default-right-margin*)
(defvar *offset* 0)
(defvar *leading* (* *font-size* *leading-ratio*))
(defvar *use-exact-char-boxes* nil)

(defvar *content* nil)

(defvar *white-chars* (coerce '(#\Space #\Tab #\Newline #\Return) 'string))

(defvar *ponctuation-marks* ".;:!?,")

(defvar *ponctuation-marks-extra-spacing-ratios*
  '((#\. 1.5 15.0 3.0 0.7 2.0)
    (#\; 1.5 15.0 3.0 0.7 2.0)
    (#\: 1.5 15.0 3.0 0.7 2.0)
    (#\! 1.5 15.0 3.0 0.7 2.0)
    (#\? 1.5 15.0 3.0 0.7 2.0)
    (#\, 1.2 12.0 3.0 0.7 2.0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym ,(symbol-name sym)))) names))
     ,@body)))

