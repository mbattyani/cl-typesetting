;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf)

(defsystem :cl-pdf-doc
  :name "cl-pdf-doc"
  :author "Brian Sorg, brian.sorg@liberatinginsight.com"
  :version "0.1"
  :maintainer "Brian Sorg, brian.sorg@liberatinginsight.com"
  :licence "BSD like licence"
  :description "Common Lisp PDF Generation Library Documentation"
  :long-description "Documentation for the stand alone lisp libraries of cl-pdf and cl-typesetting"
  :components (	(:file "package")
	       	(:file "infrastructure" :depends-on ("package"))
		(:file "chapter-1" :depends-on ("package"))
		(:file "chapter-2" :depends-on ("package"))
		(:file "chapter-3" :depends-on ("package"))
		(:file "chapter-4" :depends-on ("package"))
		(:file "chapter-5" :depends-on ("package"))
		(:file "appendix-a" :depends-on ("package"))
		(:file "document" :depends-on ("package"))
	       )
  :depends-on (:cl-pdf :cl-typesetting))