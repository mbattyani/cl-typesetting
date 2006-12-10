;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package :cl-user)

(defpackage :typeset
  (:nicknames :cl-typesetting)
  (:use :common-lisp :iterate)
  (:nicknames tt)
  (:export
   #:with-text-content #:with-document #:compile-text #:draw-pages #:finalize-page
   #:write-document
   #:paragraph 
   #:with-style #:set-style #:with-style
   #:table #:row  #:header-row  #:footer-row #:cell
   #:put-string #:format-string #:verbatim
   #:make-filled-vbox
   #:vspace #:hspace #:new-line #:new-page
   #:with-offset #:with-superscript #:with-subscript
   #:hrule #:image #:background-image
   #:display-formula #:fraction #:math-super-and-sub-script
   #:vbox #:hbox #:colored-box #:user-drawn-box
   #:mark-ref-point #:find-ref-point #:find-ref-point-page-number #:find-ref-point-page-data
   #:add-contextual-action #:set-contextual-variable #:push-contextual-variable
   #:pop-contextual-variable #:get-contextual-variable
   ))



