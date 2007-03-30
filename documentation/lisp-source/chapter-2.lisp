(in-package :cl-pdf-doc)
;;; cl-pdf-doc copyright 2007 Brian Sorg see license.txt for the details
;;; You can reach me at brian.sorg@liberatinginsight.com
;;; The homepage of cl-pdf-doc is here: http://www.liberatinginsight.com/open-projects.html"

(defun chapter-2 ()
  
  (chapter 
      ("Ch 2: Getting Started")

    (body-text "The following sections will define the basic process for creating a basic pdf text document. ")

    (body-text "The top level macro you will use to define a pdf document is the " (highlight "tt:with-document") " macro. This macro prepares everything for you that will be required to generate the document. So let us create our first pdf document. The following code snippet will create a pdf document containing one paragraph and write that document to a file with the path /tmp/first-doc.pdf. The source code to the example can be found in the cl-pdf-doc directory under examples/first-doc.lisp")

    (code-text ("First Document: Including Text") "
 (defun first-doc (&key (file \"/tmp/first-doc.pdf\"))
  (tt:with-document
      ()
    (let ((content (tt:compile-text
		       ()
		     (tt:paragraph () \"Generated with Cl-Pdf and Cl-typesetting\"))))
      (tt:draw-pages content)
      (when pdf:*page* (typeset:finalize-page pdf:*page*))
      (tt:write-document file))))
  
")

    (body-text "So lets explain the details of what is occuring with this example. First, we see the use of the " (highlight "tt:with-document") " macro. The macro instantiates the objects and initializes the parameters required to produce the document. The second step is to prepare the content of the document.  This is achieved using the " (highlight "tt:compile-text") " macro. The compile-text macro sets the style parameters such as font, font-size, font-color, and background-color to the global settings for all of the content that is contained within the macro.  It, in a sense, prepares the following text for inclusion in the pdf document. The next step is to take the prepared content and create pages in the document with it. This is accomplished with the " (highlight "tt:draw-pages") " function. It initializes the settings for the size, orientation and margins of the page; it will include any headers or footers that were provided; and will calculate as needed the page breaks required to include the content on the page. The next line ensures that the final page in the pdf document has been properly marked as finished and will not remain open waiting for more material. The last statement " (highlight "tt:write-document") " takes the prepared pages and writes them to the pdf document. The end result of this function then is a pdf file with one paragraph at the top of the page that says " (emphasize "Generated with Cl-pdf and Cl-typesetting"))
    
    (body-text "Now lets extent this example by make some changes to the font settings. Before we do this though, it is necessary to understand some of the basics of pdf fonts. First, since pdf documents are supported acrossed a broad range of operating systems the document format could not rely on the font libraries provided by the various operating systems. To overcome this problem they created a small set of standard fonts that every pdf reader on every operating system is required to have, and they created a mechanism for attaching a font type to a specific document is specialized fonts were requires. Once a font is attached to a document all readers will have the information they require to display the text in this font to the reader. Cl-pdf supports both the standard and embedded fonts. Here hower we will only take a look at the standard fonts.")

    (body-text "The standard fonts for pdf documents can all be found in the Cl-pdf code base under the directory called afm. In this directory you will find the font definition files. In practical terms there are only three font types, Times-Roman, Helvetica, and Courier and four font styles for each type, standard, bold, italic, and bold and italic.")

    (body-text "Using the tt:paragraph function you can change the font simply by providing the keywords " (emphasize ":font") " and " (emphasize ":font-size.") " For example by adding the following lines to the first-doc function after the initial paragraph statement, you can experiment with 3 new font types and sizes")

    (code-text ("Example 2: Font Types and Sizes")
      "
   (tt:paragraph (:font \"Courier\" :font-size 16) \"Courier Font size 16\")
   (tt:paragraph (:font \"Times-Bold\" :font-size 8) \"Times in bold with font size 8\")
   (tt:paragraph (:font \"Helvetica-Oblique\" :font-size 14) \"Helvetica italic font size 14.5\")
")
    (body-text "As you can see from the example, the font types are defined by using the file name found in the afm directory without the .afm extension. The font-size is defined in pts, where 72 pts is equivalent to 1 inch. Decimals are permitted.") 

    (body-text "Finally, lets look at generating some output to include in the document. In general, dynamic content may be added simply by generating a string within the context of a typesetting document formating structure. To demonstrate this, we will add numbered paragraphs to the first-document example started above. Simply add the following code snippet after the last paragraph declaration but within the compile-text macro.")

    (code-text ("First Document: Generating Content") "
(dotimes (i 10)
  (tt:paragraph ()
    \"Paragraph # \" (tt:format-string \"~d\" i)))
"
	       )

    (body-text "In this example ten paragraphs are created, each followed by the number of the paragraph.")

    (body-text "That covers all of the basics required to get started with Cl-Typesetting. From here on out it is just a matter of learning the different macros and functions available to help arrange and format the contents of your documents. These explanations following (or will follow depending on when you are reading this) in Chapters 3 and 4.")
    

    (outline-header ("More Information")
      
      (body-text "As one progresses through this documentation it will also be helpful to review examples from other sources. Both Cl-pdf and Cl-typesetting include a number of examples for one to try out and experiment with. They can be found in the source directories. For Cl-Pdf they are in the examples/examples.lisp file. In Cl-typesetting they are in the test.lisp file. You may also want to review the source code used to generate this documentation. It is found under the doc/source in the cl-pdf directory. Also the source for the examples used through out the documentation are located in under examples in the doc direction.")

      (body-text "If you are interested in understanding how a PDF document is constructed, you can review the file specification yourself. At the time of this writing Adobe published it under http://www.adobe.com/devnet/pdf/pdf_reference.html."))))