(in-package :cl-pdf-doc)
;;; cl-pdf-doc copyright 2007 Brian Sorg see license.txt for the details
;;; You can reach me at brian.sorg@liberatinginsight.com
;;; The homepage of cl-pdf-doc is here: http://www.liberatinginsight.com/open-projects.html"

(defmacro chapter ((title &key (outline-name title)) &body body)
  "Sets up a block of code that will represent a chapter in the documentation. 
Title - Is the title of the chapter
Outline-name - is the pdf bookmark name
Body - Chapter Contents"
  `(let ((content (tt:compile-text 
		      ()
		    (pdf:with-outline-level (,outline-name (pdf:register-page-reference))
		      (header ,title 1)
		      ,@body)
		    (tt:new-page))))
    (tt:draw-pages  content :margins *doc-margins* :header (generate-header) :footer (generate-footer)
     :size (rest (assoc :letter tt::+paper-sizes+)))))

(defmacro outline-header ((header-string &key (header-size 1) (outline-name header-string)) &body body)
  "Header for the text that will be registered in the pdf bookmarks outline section"
  `(pdf:with-outline-level (,outline-name (pdf:register-page-reference))
    (header ,header-string ,header-size)
    ,@body))

(defmacro header (header-string header-size)
  "Header Block of Text
Header-Size - Relates to the importance of the header. 1 Signals the most important, larger numbers drop in significance"
  `(tt:paragraph (:font "Helvetica-Bold" :font-size (get-header-font ,header-size))
    (tt:vspace 5)
    ,header-string 
    (tt:vspace 5)))

(defmacro body-text (&body body)
  "Normal Body Text"
  `(tt:paragraph (:font "Helvetica" :font-size 10 :top-margin 3 :bottom-margin 3)
    ,@body))

(defmacro code-text ((&optional (title "Code Example")) &body body)
  "Verbatim Lisp code text"
  `(progn 
    (tt:paragraph (:font "Helvetica" :font-size 10 :top-margin 10  :left-margin 36 :right-margin 36)
      (tt:verbatim ,@body))
    (tt:paragraph (:font "Helvetica-Bold" :font-size 10 :h-align :center :left-margin 36 :right-margin 36 :bottom-margin 10)
      ,title)))

(defmacro quote-block (&body body)
  "Block of text which is set apart by font type and extra indentation"
  `(tt:paragraph (:font "Times-Roman" :font-size 10 :top-margin 5 :bottom-margin 5 :left-margin 36 :right-margin 36)
    ,@body))

(defmacro highlight (&body body)
  "Place body text inline in bold"
  `(tt:with-style (:font "Helvetica-Bold" :font-size 10)
    ,@body))

(defmacro emphasize (&body body)
  "Place body test inline in italics"
  `(tt:with-style (:font "Helvetica-Oblique" :font-size 10)
    ,@body))

(defun definition-table (definition-lists)

  (tt:table (:col-widths '(36 150 300) :border 0 :splittable-p t)
    (dolist (def definition-lists)
      (tt:row ()
	(tt:cell () (tt:put-string ""))
	(tt:cell () (highlight (tt:put-string (first def))))
	(tt:cell () (body-text (tt:put-string (second def))))))))

(defun get-header-font (size)
  "Given a default size return the documents font size"
  (case size
    (1 18)
    (2 16)
    (3 14)
    (4 12)
    (5 10)))

(defun generate-header ()
  "Page Header Code"
  (tt:compile-text 
      ()
    (tt:paragraph (:h-align :center
				 :font "Helvetica-BoldOblique" :font-size 14)
      "Cl-Pdf Documentation - Version 0.1" :eol
      (tt:vspace 2)
      (tt:hrule :dy 1))))

(defun generate-footer ()
  "Page Footer Code"
  (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
			  (get-decoded-time)
			(declare (ignore second minute hour))
			(format nil "Generated on ~4D-~2,'0D-~2,'0D" 
				year month date))))
    (lambda (pdf:*page*)
      (tt:compile-text 
	  (:font "Helvetica" :font-size 10)
	(tt:paragraph (:h-align :center  :font "Helvetica" :font-size 10)
	  (tt:hrule :dy 1)
	  (tt:vspace 2)
	  (tt:put-string "Brian Sorg, Founder Liberating Insight LLC") :eol
	  (tt:put-string "Copyright Fort Wayne IN USA 2007"):eol
	  (tt:verbatim print-stamp)
	  (tt:verbatim 
	   (format nil "   Page ~a" pdf:*page-number*)))))))


(defmacro function-description ((name &key (type "function") (spec "()") (return-value "nil") (args nil)) &body description)
  "Macro to set up a macro or function description"
  (let ((line (gensym)))
    `(pdf:with-outline-level (,name (pdf:register-page-reference))
      (header (tt:put-string (string-upcase ,name)) 4)
      (header "Syntax" 5)
      (body-text ,type " " (highlight ,name) " " (emphasize ,spec) " ==> " ,return-value)
      (when ,args
	(header "Arguments" 5)
	(dolist (,line ,args)
	  (if (listp ,line)
	      (tt:paragraph (:font "Helvetica" :font-size 10 :left-margin 40)
		(emphasize (tt:put-string (first ,line))) (tt:put-string (format nil ": ~a" (second ,line))))
	      (tt:paragraph (:font "Helvetica" :font-size 10 :left-margin 20)
		(highlight (tt:put-string ,line))))))
      (header "Description" 5)
      ,@description)))