(in-package :cl-pdf-doc)

;;; Will contain cl-typesetting formatting explanations 

(defun chapter-3 ()
  
  (chapter 
      ("Ch 3: Document & Layout Functions")
    
    (function-description 
	("tt:with-document" :type "macro" :spec "(&rest args) &body body"
			    :args '("&rest"
				    (":author" "Author of the document.")
				    (":title" "Documents Title")
				    (":keywords" "Keywords of the document")
				    (":subject" "Subject of the document")
				    "&body"
				    ("body" "Code generating the document")))
      (body-text "The " (emphasize "with-document") " macro is the starting point for all documents. It is responsible for initializing all of the required objects need to generate the document. The arguments for author, title, keywords, subject are filled in a meta data for the document, and will appear under the document properties listings."))

    (function-description
	("tt:write-document" :type "defmethod" :spec "output-location &optional document" 
			     :args '("Required"
				     ("output-location" "string, pathname or stream, determines where the document content should be sent")
				     "&Optional"
				     ("document" "default *document, normal users will not need to provide this value")))
      (body-text "Used to write out the given content of the document"))

    (function-description 
	("tt:draw-pages" :spec "content &rest args &key (size *default-page-size*) (orientation *default-page-orientation*) bounds margins header (header-top *default-page-header-footer-margin*) footer (footer-bottom *default-page-header-footer-margin*)  break finalize-fn &allow-other-keys)"
			 :args '("Required"
				 ("content" "A section of the doucment prepared by the macro compile-text")
				 "&key"
				 ("size" "Dimensions of the document's page")
				 ("orientation" "Orientation of the document")
				 ("bounds" "Media box; overwrites size and orientation when specified.")
				 ("margins" "White space between the page's edge and the text, list of 4 numbers defining left-margin top-margin right-margin bottom-margin")
				 ("header"  "Content or function of ftype (function (page) content which defines the header content of each page")
				 ("header-top" "Distance between the top media edge and the header.")
				 ("footer" "Content or function of ftype (function (page) content which defines the footer content of each page")
				 ("footer-bottom" "Distance between the bottom media edge and the footer.")
				 ("break" "Force new page ::= :before | :after | :always (both ends)")))
      (body-text "Top level function useful for generating a multi-page section of the document. This function will start generating the content and will automatically overflow content onto another page as required to include all of the content in the document."))

    (function-description 
	("tt:paragraph" :type "macro" :spec "(&rest style) &body body" 
			:args '("&rest"
				("style" "These are all of the text style markers, used in the paragraph. The common markers are defined as follows")
				(":top-margin" "space between the text of paragraph and the preceding document content")
				(":bottom-margin" "space between the content of the paragraph and the following document content")
				(":first-line-indent" "Number, the indentation of the first line of the paragraph, default is 0")
				(":font" "designator of which font to utilize in this paragraph")
				(":font-size" "size of the font to employ")
				("text-x-scale" "")
				(":color" "Foreground/text color")
				(":background-color" "")
				("h-align" "Content alignment ")
				("left-margin" "")
				("right-margin" "")
				("pre-decoration" "")
				("post-decoration" "")))
      (body-text "Defines a paragraph with the given style set for the contents of the paragraph."))

    

    ))