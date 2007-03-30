(in-package :cl-pdf-doc)
;;; cl-pdf-doc copyright 2007 Brian Sorg see license.txt for the details
;;; You can reach me at brian.sorg@liberatinginsight.com
;;; The homepage of cl-pdf-doc is here: http://www.liberatinginsight.com/open-projects.html"

(defun chapter-1 ()
  
  (chapter 
      ("Introduction")

    (body-text "Cl-Pdf and Cl-Typesetting are cross-platform stand-alone Common Lisp Libraries for producing PDF documents. The libraries are overseen by Marc Battyani of Fractal Concept and can be downloaded from their website")
    (quote-block "http://www.fractalconcept.com.")
    (body-text "Cl-Pdf and Cl-Typesetting are both released with a FreeBSD style license making them usable for commercial work.")

    (body-text "For the purpose of this documentation it will be assumed that Cl-Pdf and Cl-Typesetting are really one module used in conjunction to produce PDF documents. In reality Cl-Pdf is the base library for producing PDF documents and has capabilities similiar to that of a CAD system, ie manually positioning text boxes on the page and then filling them with content. As a follow on Cl-Typesetting builds on Cl-Pdf offering a complete typesetting system. Therefore most users will focus on implementing the functionality of CL-Typesetting and will only utilize Cl-Pdf directly when producing some specific document features.")

    (body-text "This document is intended it help programmers utilize Cl-Pdf and Cl-Typesetting to produce pdf documents from their Lisp applications. It is not a developer's guide to the source code for Cl-Pdf and Cl-Typesetting. If you have a specific question about the code or would like to contribute please use the cl-pdf-dev mailing lists to post your contributions and questions.")
        
    (body-text "Also note that the documentation is not intended to function as a stand alone document. Instead being entirely produced utilizing Cl-Pdf and Cl-Typesetting, it is designed to be read in conjuction with the source code that produced it. Therefore you will find very few code examples in the text. If you wish to see how something was generated, take a look at the source code that produced the document. It is logically broken up starting with the main function being located in the document.lisp file, each chapter being contained in a file named chapter-*.lisp and document wide functions and macros being found in the infrastructure.lisp file")

    (outline-header ("Obtaining")
      (body-text "Cl-Pdf and Cl-Typesetting can be downloaded from the Fractal Concept company website at: ")
      (quote-block "http://www.fractalconcept.com.")
      (body-text "Tarball versions of the lastest stable releases are available, but it is recommended that you download the latest code from the Subversion repository. Directions of how to do this can be found on the website"))

    (chapter-1-installation)

    ))

(defun chapter-1-installation ()
  (outline-header ("Installation")
    
    (body-text "To install Cl-Pdf and Cl-Typesetting, it is recommended that you use the asdf system definitions provided with the Cl-Pdf and the Cl-Typesetting packages. If you are not familiar with the asdf packaging system you can download it and its documentation from")
    (quote-block "http://www.cliki.net/asdf")

    (body-text "Once you have loaded the asdf package. You can compile and load CL-Pdf and Cl-Typesetting with the following commands:")

    (quote-block (dolist (line '("(push #P\"<your path>/cl-pdf/\" asdf:*central-registry*)" 
				 "(asdf:operate 'asdf:load-op 'cl-pdf)" 
				 "(push #P\"<your path>/cl-typesetting/\" asdf:*central-registry*)"
				 "(asdf:operate 'asdf:load-op 'cl-typesetting)"))
		   (tt:put-string line) (tt:new-line)))

    (body-text "As part of loading the system there are a few important parameters that you will want to specify that will effect the default behavior of documents produced. The first ones are found in the Cl-Pdf package and are defined in the config.lisp file. There are many parameters found in the file, but the most important ones are:")


    (definition-table '(("*max-number-of-pages*" "The maximum number of pages a document may contain before it stops. Default is 1000")
			("*default-page-bounds*" "What dimensions a default page in Cl-Pdf should have. The default defines an A4 size page with portrait orientation, options include  *a4-portrait-page-bounds*, *letter-portrait-page-bounds*, *a4-landscape-page-bounds*, *letter-landscape-page-bounds*.")))

    

    (body-text "The other parameters you should become familiar with are found in the Cl-Typesetting package and are located in the top-level and specials files. Once again there are a number of parameters which you can use to customize your pdf documents. However, make sure that you have properly configured the following ones:")

    
    (definition-table '(("*default-page-size*" "Controls the default page dimensions for Cl-Typesetting capabilities. Options include: :A3, :A4, :A5, :Letter, :Legal. The default is :A4")
			("*default-page-orientation*" "Controls the default page orientation. Options are :portrait and :landscape. The default is :portrait")
			("*default-font*" "Controls the default font type, The default is Helvetica")
			("*default-font-size*" "Controls the default font size, The default is 12")))))


		 

    

