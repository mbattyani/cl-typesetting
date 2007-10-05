;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package #:typeset)

#+(and clisp win32)
(setq custom:*floating-point-contagion-ansi* t
      custom:*warn-on-floating-point-contagion* nil
      custom:*default-file-encoding* (ext:encoding-charset charset:iso-8859-1))

#+(and clisp (not win32))
(setq custom:*floating-point-contagion-ansi* t
      custom:*warn-on-floating-point-contagion* nil
      custom:*default-file-encoding* (ext:encoding-charset "iso-8859-1"))

(defconstant +huge-number+ (truncate most-positive-fixnum 10))
(defconstant +epsilon+ 0.0001)

;;
;; FLAG -- collect all these in *default-text-style* and *current-text-style* ;; djc
;; Note: Don't let any of these variables become NIL, otherwise
;; that style won't be restored after a change. cf. typo.lisp
;;
(defvar *default-font* (pdf:get-font))
(defvar *default-font-size* 12.0)
(defvar *default-text-x-scale* 1)
(defvar *default-color* '(0 0 0))
(defvar *default-background-color* '(1.0 1.0 1.0))
(defvar *default-h-align* :left)
(defvar *default-v-align* :top)
(defvar *default-left-margin* 0)
(defvar *default-right-margin* 0)
(defvar *default-pre-decoration* :none)
(defvar *default-post-decoration* :none)
(defvar *default-leading-ratio* 1.2)

(defvar *font* *default-font*)
(defvar *font-size* *default-font-size*)
(defvar *text-x-scale* *default-text-x-scale*)
(defvar *color* *default-color*)
(defvar *background-color* *default-background-color*)
(defvar *h-align* *default-h-align*)
(defvar *v-align* *default-v-align*)
(defvar *left-margin* *default-left-margin*)
(defvar *right-margin* *default-right-margin*)
(defvar *pre-decoration* *default-pre-decoration*)
(defvar *post-decoration* *default-post-decoration*)
(defvar *leading-ratio* *default-leading-ratio*)
(defvar *leading* (* *font-size* *leading-ratio*))

(defvar *offset* 0)
(defvar *use-exact-char-boxes* nil)

(defvar *content* nil)

(defvar *white-chars* (coerce '(#\Space #\Tab #\Newline #\Return) 'string))

(defvar *punctuation-marks* ".;:!?,")

(defvar *punctuation-marks-extra-spacing-ratios*
  '((#\. 1.5 15.0 3.0 0.7 2.0)
    (#\; 1.5 15.0 3.0 0.7 2.0)
    (#\: 1.5 15.0 3.0 0.7 2.0)
    (#\! 1.5 15.0 3.0 0.7 2.0)
    (#\? 1.5 15.0 3.0 0.7 2.0)
    (#\, 1.2 12.0 3.0 0.7 2.0)))

(defvar *current-pass* nil)
(defvar *max-number-of-passes* 2)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym ,(symbol-name sym)))) names))
     ,@body)))

;;; Quad is construction for specifying values for margins, borders, paddings etc.
;;; It is represented as
;;;  - either four-element vector #(left top right bottom),
;;;  - or four-or-less-element list with defaulting rightmost elements,
;;;  - or number supplying the same value for all the four components.
;;; Roughly equivalent to 
;;;	(destructuring-bind (left &optional (top left) (right left) (bottom top)) quad
;;; NB: CSS2 assumes different sequence of values: (top right bottom left) !
(defmacro with-quad ((left &optional top right bottom) quad &body body)
  (with-gensyms (q)
   `(let* ((,q ,quad)
           (,left (cond ((vectorp ,q) (aref ,q 0))
                        ((consp ,q) (first ,q))
                        ((prog1 (or ,q 0) (setq ,q nil)))))
           ,@(when top    `((,top (if (vectorp ,q) (aref ,q 1) (or (second ,q) ,left)))))
           ,@(when right  `((,right (if (vectorp ,q) (aref ,q 2) (or (third ,q) ,left)))))
           ,@(when bottom `((,bottom (if (vectorp ,q) (aref ,q 3) (or (fourth ,q) ,top))))) )
      ,@body)))

(define-condition end-of-page (condition)
  ((box :initarg :box :reader box :initform nil))
  (:report (lambda (c stream)
             (format stream "Unexpected end-of-page during layout or stroking~@[ ~s~]."
                     (box c)))))

(define-condition cannot-fit-on-page (condition)
  ((box :initarg :box :reader box :initform nil))
  (:report (lambda (c stream)
             (format stream "Unable to fit object~@[ ~s~] even on a new page."
                     (box c)))))

(defmacro defconstant* (name value &optional doc)
  `(defconstant ,name
                (if (boundp ',name) (symbol-value ',name) ,value)
                ,@(when doc (list doc))))

;;; The string type to use for unicode characters

(define-symbol-macro  unicode-string-type
  #+lispworks 'lispworks:simple-text-string
  #+sbcl 'simple-string
  #+(or allegro clisp) 'simple-base-string
  #-(or lispworks sbcl clisp allegro) 'string)

