;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package "CL-USER")

(defpackage typeset
  (:use common-lisp)
  (:export
   #:with-text-content
   #:paragraph
   #:with-style #:set-style #:with-style
   #:table #:row #:cell
   #:put-string
   #:make-filled-vbox
   #:vspace #:hspace
   #:with-offset #:with-superscript #:with-subscript
   #:hrule #:image #:background-image
   #:display-formula #:fraction #:math-super-and-sub-script
   #:vbox #:hbox 
   ))



