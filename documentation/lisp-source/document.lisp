(in-package :cl-pdf-doc)
;;; cl-pdf-doc copyright 2007 Brian Sorg see license.txt for the details
;;; You can reach me at brian.sorg@liberatinginsight.com
;;; The homepage of cl-pdf-doc is here: http://www.liberatinginsight.com/open-projects.html"

(defparameter *doc-margins* (list 36 72 36 72) "Margins used throughout document 72 = 1 inch or 0.5 in left and right, 1 in top and bottom")
(defparameter *doc-paper-size* (rest (assoc :letter tt::+paper-sizes+)) 
  "Options can be found in typesetting->top-level.lisp, and are currently :A3, :A4, :A5, :Letter :Legal")

(defun generate-cl-pdf-documentation (&key (file "/tmp/cl-pdf-doc.pdf"))

  (tt:with-document  
      (:author "Brian Sorg, Founder Liberating Insight LLC" :title "Cl-Pdf Documentation" :keywords "Cl-Pdf, Cl-Typesetting" :subject "Cl-Pdf User Document")
    (title-page)
    (chapter-1)
    (chapter-2)
    (chapter-3)
    (appendix-a)
    (when pdf:*page* (typeset:finalize-page pdf:*page*))
    (tt:write-document file)))

(defun title-page ()

  (let ((content (tt:compile-text 
		     ()
		   (pdf:with-outline-level ("Title Page" (pdf:register-page-reference))
		     (tt:paragraph (:font "Helvetica-Bold" :font-size 22 :h-align :center)
		       "Cl-Pdf And Cl-Typesetting" :eol
		       "Users Documentation" :eol
		       (tt:vspace 400))
		     (body-text "Written by: Brian Sorg, Founder Liberating Insight LLC" :eol
				"brian.sorg@liberatinginsight.com")
		     (tt:new-page)))))
    (tt:draw-pages  content :margins *doc-margins* 
		    :size (rest (assoc :letter tt::+paper-sizes+)))))

