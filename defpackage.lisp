;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package "CL-USER")

(defpackage typeset
  (:use common-lisp)
  (:nicknames tt)
  (:export
   #:with-text-content
   #:paragraph
   #:with-style #:set-style #:with-style
   #:table #:row #:cell
   #:put-string #:verbatim
   #:make-filled-vbox
   #:vspace #:hspace #:new-line #:new-page
   #:with-offset #:with-superscript #:with-subscript
   #:hrule #:image #:background-image
   #:display-formula #:fraction #:math-super-and-sub-script
   #:vbox #:hbox #:colored-box #:user-drawn-box 
   ))



