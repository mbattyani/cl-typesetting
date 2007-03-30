(in-package :cl-pdf-doc)
;;; cl-pdf-doc copyright 2007 Brian Sorg see license.txt for the details
;;; You can reach me at brian.sorg@liberatinginsight.com
;;; The homepage of cl-pdf-doc is here: http://www.liberatinginsight.com/open-projects.html"


(defun appendix-a ()

  (chapter 
      ("Appendix A")

    (outline-header ("Parameters")
      (display-parameters))))

(defun display-parameters ()
  (let (;; Form -- Parameter name, package, definition
	;; Please enter in Alphabetical Order.
	(ps '(("*default-font*" "type" "Controls the default font type, The default is Helvetica")
	      ("*default-font-size*" "type" "Controls the default font size, The default is 12")
	      ("*default-page-bounds*" "pdf" "What dimensions a default page in Cl-Pdf should have. The default defines an A4 size page with portrait orientation, options include  *a4-portrait-page-bounds*, *letter-portrait-page-bounds*, *a4-landscape-page-bounds*, *letter-landscape-page-bounds*.")
	      ("*default-page-orientation*" "type" "Controls the default page orientation. Options are :portrait and :landscape. The default is :portrait")
	      ("*default-page-size*" "type" "Controls the default page dimensions for Cl-Typesetting capabilities. Options include: :A3, :A4, :A5, :Letter, :Legal. The default is :A4")
	      ("*max-number-of-pages*" "pdf" "The maximum number of pages a document may contain before it stops. Default is 1000"))))

    (tt:table (:col-widths '(100 50 300) :splittable-p t :border 0)
      (dolist (p ps)
	(tt:row ()
	  (tt:cell () 
	    ;; provide mechanism to link references to these definitions in the name body of the text???
	    (pdf::register-named-reference (first p))
	    (highlight (tt:put-string (first p))))
	  (tt:cell () (body-text (tt:verbatim (second p))))
	  (tt:cell () (body-text (tt:put-string (third p)))))))))