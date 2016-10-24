;; wrapper around ceteicean


;; main idea: take a buffer containing a TEI xml document, and serve
;; it as an html document with ceteican.

(require 'web-server);; https://github.com/eschulte/emacs-web-server
;; caused trouble?: curl: (18) transfer closed with 1 bytes remaining to read
;; ... on all pages

;; (require 'simple-httpd);; https://github.com/skeeto/emacs-web-server

(defun teiwal/html-buffer-name (tei-buffer)
  "Return name of buffer that should contain the html for TEI-BUFFER."
  (let ((name (cond ((bufferp tei-buffer) (buffer-name tei-buffer))
		    ((stringp tei-buffer) tei-buffer)
		    (t (error "Not a buffer or string: %s" tei-buffer))))
	(teiwal-prefix "* teiwal - ")
	(teiwal-postfix " *"))
    (concat teiwal-prefix name teiwal-postfix)))

;; (teiwal/html-buffer-name "soup") ;;"* teiwal - soup *"
;; (teiwal/html-buffer-name (current-buffer));; "* teiwal - teiwal.el *"

;; build the html file: header + TEI document in div + rest

(defun teiwal/html-from-buffer (buffer)
  "Turn BUFFER into an html document."
  (let ((tei-doc buffer)
	(html-buffer (get-buffer-create (teiwal/html-buffer-name buffer))))
    (with-current-buffer html-buffer
      (insert
       (xml-to-string
	'((html nil
		(head nil
		      (meta
		       ((charset . "utf-8")))
		      (link
		       ((rel . "stylesheet")
			(href . "http://teic.github.io/CETEIcean/css/CETEIcean.css")
			(media . "screen")
			(charset . "utf-8"))))
		(body nil "\n    "
		      (div
		       ((id . "TEI"))
		       "\n      Sadly, this page will not work in Internet Explorer and some older browsers. We suggest you use a newer version of Chrome or Firefox.\n    ")
		      "\n    "
		      (script
		       ((src . "https://github.com/TEIC/CETEIcean/releases/download/v0.3.2/CETEI.js")))
		      (script nil "\n      var CETEIcean = new CETEI();\n      CETEIcean.getHTML5('testTEI.xml', function(data) {\n        document.getElementById(\"TEI\").innerHTML = \"\";\n        document.getElementById(\"TEI\").appendChild(data);\n        CETEIcean.addStyle(document, data);\n      });\n\n      // Alternatively, use then()\n      // (new CETEI).getHTML5('testTEI.xml').then(function(data){\n      //   document.getElementById(\"TEI\").appendChild(data);\n      // });\n\n    "))))))
      (insert-buffer-substring tei-doc (point-min) (point-max))
      )
    )
  )


;; from http://teic.github.io/CETEIcean/simpleTest.html:


;; '(html nil
;;       (head nil
;; 	    (meta
;; 	     ((charset . "utf-8")))
;; 	    (link
;; 	     ((rel . "stylesheet")
;; 	      (href . "css/CETEIcean.css")
;; 	      (media . "screen")
;; 	      (charset . "utf-8"))))
;;       (body nil "\n    "
;; 	    (div
;; 	     ((id . "TEI"))
;; 	     "\n      Sadly, this page will not work in Internet Explorer and some older browsers. We suggest you use a newer version of Chrome or Firefox.\n    ")
;; 	    "\n    "
;; 	    (script
;; 	     ((src . "js/CETEI.js")))
;; 	    (script nil "\n      var CETEIcean = new CETEI();\n      CETEIcean.getHTML5('testTEI.xml', function(data) {\n        document.getElementById(\"TEI\").innerHTML = \"\";\n        document.getElementById(\"TEI\").appendChild(data);\n        CETEIcean.addStyle(document, data);\n      });\n\n      // Alternatively, use then()\n      // (new CETEI).getHTML5('testTEI.xml').then(function(data){\n      //   document.getElementById(\"TEI\").appendChild(data);\n      // });\n\n    ")))


;;; server things

(defcustom teiwal/listen-address "127.0.0.1"
  "Network address to listen on."
  :group 'teiwal
  :type 'string)

(defcustom teiwal/listen-port 9003
  "Network port to listen on."
  :group 'teiwal
  :type 'integer)

(defcustom teiwal/log-buffer-name "*teiwal server log*"
  "Name of buffer to log to."
  :group 'teiwal
  :type 'string)

(defun teiwal/log-buffer ()
  "Return log buffer for this."
  (get-buffer-create teiwal/log-buffer-name))

(defun teiwal/server-start ()
  "Start a teiwal server."
  (interactive)
  (message "Starting server %s:%s" teiwal/listen-address teiwal/listen-port)
  (ws-start
   '(((:GET . ".*") . teiwal/serve-buffer))
   teiwal/listen-port
   ;; log buffer
   (teiwal/log-buffer)
   ;; NETWORK-ARGS --> passed to `make-network-process'
   :host teiwal/listen-address))


(defun teiwal/server-stop ()
  "Stop all running servers."
  (interactive)
  (ws-stop-all))

(defun teiwal/get-nxml-buffers ()
  "Return a list of names of buffers in nxml-mode."
  (delq nil
	(mapcar (lambda (x)
		  (with-current-buffer x
		    (when (eq major-mode 'nxml-mode)
		      (buffer-name x))))
	   (buffer-list))))


(defun teiwal/sxml-to-string (sxml)
  "Convert SXML to string.

SXML: result of `libxml-parse-html-region' or `libxml-parse-xml-region'.

See https://github.com/tali713/esxml/blob/master/esxml.el for
similar stuff."
  (cond
   ((null sxml) '())
   ((stringp sxml) sxml)
   (t
    (pcase-let ((`(,tag ,attrs . ,body) sxml))
      (concat "<" (symbol-name tag)
              (when attrs
                (concat " " (mapconcat (lambda (att)
					 (format "%s=\"%s\"" (car att) (cdr att)))
				       attrs " ")))
              (if body
                  (concat ">" (mapconcat 'teiwal/sxml-to-string body "")
                          "</" (symbol-name tag) ">")
                "/>"))))))

(defun teiwal/serve-buffer (request)
  (with-slots (process headers) request
    (let ((path (substring (cdr (assoc :GET headers)) 1)))
      (cond
       ((string= path "")
	(with-temp-buffer
	  (insert "<!DOCTYPE html>\n")
	  (insert
	   (teiwal/sxml-to-string
	    `(html ()
		   (head ()
			 (title
			  ()
			  "TEIwal main window")
			 (meta
			  ((charset . "utf-8"))))
		   (body ()
			 (ol ()
			     ,(mapconcat
			       (lambda (buf)
				 (teiwal/sxml-to-string
				  (list 'li '()
					(list
					 'a
					 (list
					  (cons 'href (format "/%s" (url-hexify-string buf))))
					 buf))))
			       (teiwal/get-nxml-buffers)
			       "\n"))
			 ))))
	  ;; (ws-response-header proc 200
	  ;; 		      (cons "Content-type" "text/html")
	  ;; 		      (cons "Content-length" (position-bytes (point-max))))
	  (ws-response-header proc 200
			      '("Content-type" . "text/html"))
	  (ws-send proc (buffer-string))))
       ((member path (teiwal/get-nxml-buffers))
	(with-temp-buffer
	  (insert "<!DOCTYPE html>\n")
	  (insert
	   (teiwal/sxml-to-string
	    `(html ()
		   (head ()
			 (meta
			  ((charset . "utf-8")))
			 "\n"
			 ;;; add webcomponents first
			 ;; (script
			 ;;  ((src . "/js/webcomponentsjs-0.7.22/webcomponents.min.js"))
			 ;;  "\n// polyfill stuff: https://github.com/webcomponents/webcomponentsjs \n")
			 (script
			  ((src . "/CETEIcean/dist/CETEI.js"))
			  "\n// See http://teic.github.io/CETEIcean\n")
			 (script
			  ((src . "/js/sarit.js"))
			  "\n// SARIT specific extensions \n")
			 
			 (link
			  ((rel . "stylesheet")
			   (href . "https://necolas.github.io/normalize.css/5.0.0/normalize.css")
			   (media . "screen")
			   ;; (charset . "utf-8")
			   ))
			 (link
			  ((rel . "stylesheet")
			   (href . "/CETEIcean/test/CETEIcean.css")
			   (media . "screen")
			   ;; (charset . "utf-8")
			   ))
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/sarit.css")
			   (media . "screen")
			   ;; (charset . "utf-8")
			   ))
			 (title
			  ()
			  ,(format "TEIwal for: %s" path)))
		   (body () "\n    "
			 (div
			  ((id . "TEI"))
			  "\n      Trying to load file ... (This page will not work in Internet Explorer and some older browsers. We suggest you use a newer version of Chrome or Firefox.)\n    ")
			 "\n    "
			 (script ()
				 ,(format
				   "saritSetup('buffer/%s');"
				   (url-hexify-string path)))))))
	  (ws-response-header proc 200
			      (cons "Content-type" "text/html"))
	  (ws-send proc (buffer-string))))
       ((and
	 (string-match "^buffer/\\(.+\\)$" path)
	 (member (match-string 1 path) (teiwal/get-nxml-buffers)))
	(with-current-buffer (get-buffer (match-string 1 path))
	  (ws-response-header proc 200
			      (cons "Content-type" "application/xml"))
	  (ws-send proc (buffer-string))))
       ((file-exists-p (expand-file-name path "/home/beta/webstuff/emacs-things/teiwal/"))
	(ws-send-file proc (expand-file-name path "/home/beta/webstuff/emacs-things/teiwal/")))
       (t
	(ws-response-header proc 404  '("Content-type" . "text/plain"))
	(process-send-string proc "Ahem, not found?"))))))


(defun teiwal/template-send-file (proc path &optional mime-type template-vars)
  "Send PATH to PROC.
Optionally explicitly set MIME-TYPE, otherwise it is guessed by
`mm-default-file-encoding'.

An extremely optimistic templating system, ripped off from `ws-send-file'."
  (let ((mime (or mime-type
                  (mm-default-file-encoding path)
                  "application/octet-stream")))
    ))
