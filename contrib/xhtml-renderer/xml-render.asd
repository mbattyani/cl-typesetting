;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; How to use this:
;; 
;; Get Marc Battyani's "cl-typesetting" and "cl-pdf" packages:
;;    http://www.fractalconcept.com/asp/html/cl-typesetting.html
;;
;; and Miles Egan's xmls parser:
;;    http://common-lisp.net/project/xmls/
;;
;; Then load this package and use as follows:
;;   (tt::xhtml-to-pdf "everything.html" "/tmp/output.pdf")
;;
;; If you have clisp, you may want to use the included shell script
;; "html2pdf" for command line use. Read the script comments for more details.

(in-package :asdf)

(defsystem :xml-render
  :name "xml-render"
  :author "Klaus Weidner <klaus@atsec.com>"
  :version "2.1.1"
  :maintainer "Klaus Weidner <klaus@atsec.com>"
  :licence "BSD like license"
  :description "none"
  :long-description ""
  :perform (load-op :after (op xml-render)
                    (pushnew :xml-render cl:*features*))
  :components ((:file "xml-xform"))
  :depends-on (:cl-typesetting
	       :xmls))
