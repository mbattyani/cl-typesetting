;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package :common-lisp-user)

(defpackage #:cl-typesetting-system
    (:use #:cl #:asdf))

(in-package #:cl-typesetting-system)

(defsystem :cl-typesetting
    :name "cl-typesetting"
    :author "Marc Battyani <marc.battyani@fractalconcept.com>"
    :version "0.8"
    :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
    :licence "BSD like licence"
    :description "Common Lisp Typesetting system"
    :long-description "The cl-typesetting package is a stand-alone Common Lisp typesetting system."
    :perform (load-op :after (op cl-typesetting)
		      (pushnew :cl-typesetting cl:*features*))
    :components ((:file "defpackage")
		 (:file "specials" :depends-on ("defpackage"))
		 (:file "boxes" :depends-on ("specials"))
		 (:file "boxes-fn" :depends-on ("boxes"))
		 (:file "graphics" :depends-on ("boxes"))
		 (:file "typo" :depends-on ("boxes"))
		 (:file "math" :depends-on ("boxes"))
		 (:file "hyphenation-fp" :depends-on ("specials"))
		 (:file "hyphenation" :depends-on ("boxes" "hyphenation-fp"))
		 (:file "layout" :depends-on ("typo" "graphics"))
		 (:file "tables" :depends-on ("layout"))
		 (:file "stroke" :depends-on ("layout"))
		 (:file "references" :depends-on ("specials"))
		 (:file "top-level" :depends-on ("stroke" "typo" "references"))
		 (:file "kw-extensions" :depends-on ("top-level" "hyphenation"))
;		 (:file "test" :depends-on ("top-level" "tables" "math"))
		 (:file "pprint" :depends-on ("top-level"))
		 )
    :depends-on (:cl-pdf))

