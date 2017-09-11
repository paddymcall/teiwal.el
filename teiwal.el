#! /bin/sh
":"; exec emacs --quick --script "$0" -- "$@" # -*-emacs-lisp-*-

;;; teiwal.el --- Serve (TEI) XML buffers with ceteicean

;; Copyright (C) 2016 Patrick McAllister

;; Author: Patrick McAllister <pma@rdorte.org>
;; Keywords: TEI, xml, server, ceteicean

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a wrapper around ceteicean
;; (http://teic.github.io/CETEIcean/,
;; https://github.com/TEIC/CETEIcean) that lets you easily preview TEI
;; XML files.  

;; standard emacs stuff
(require 'autorevert)
(require 'files)

;; extras

;; https://github.com/eschulte/emacs-web-server
(if (featurep 'web-server)
    (require 'web-server)
  (add-to-list 'load-path "emacs-web-server/")
  (require 'web-server))


(defvar teiwal/server-process nil
  "Variable to store the server process in.")

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

(defcustom teiwal/listen-port 9004
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

;;;###autoload
(defun teiwal/server-start (&optional interactive)
  "Start a teiwal server."
  (interactive '(t))
  (message "Starting server on http://%s:%s" teiwal/listen-address teiwal/listen-port)
  (setq teiwal/server-process
	(ws-start
	 '(((:GET . ".*") . teiwal/serve-buffer))
	 teiwal/listen-port
	 ;; log buffer
	 (teiwal/log-buffer)
	 ;; NETWORK-ARGS --> passed to `make-network-process'
	 :host teiwal/listen-address))
  (when interactive
    (browse-url (format "%s:%s" teiwal/listen-address teiwal/listen-port)))
  teiwal/server-process)


(defun teiwal/server-stop ()
  "Stop all running servers."
  (interactive)
  (setq teiwal/server-process (ws-stop teiwal/server-process))
  (unless (null teiwal/server-process)
    (error "Process still running: %s" teiwal/server-process)))

(defun teiwal/get-nxml-buffers ()
  "Return a list of names of buffers in nxml-mode."
  (delq nil
	(mapcar (lambda (x)
		  (with-current-buffer x
		    (when (or (eq major-mode 'nxml-mode)
			      (string=
			       "xml"
			       (downcase
				(format "%s" (file-name-extension (buffer-name x))))))
		      (buffer-name x))))
		(buffer-list))))

(defun teiwal/get-nxml-buffers-filenames ()
  "Return a list of filenames for buffers in nxml-mode."
  (delq nil
	(mapcar
	 (lambda (buf) (when (and (get-buffer buf)
				  (buffer-file-name (get-buffer buf)))
			 (with-current-buffer (get-buffer buf)
			   (expand-file-name (buffer-file-name (current-buffer))))))
	 (teiwal/get-nxml-buffers))))

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
    (let ((path	;; we need to decode to find in buffer list
	   ;; (decode-coding-string (url-unhex-string "tattvasa%E1%B9%83grahapa%C3%B1jik%C4%81.xml")
	   ;; 			 'utf-8);; "tattvasaṃgrahapañjikā.xml"
	   (decode-coding-string
	    ;; (url-unhex-string "tattvasa%E1%B9%83grahapa%C3%B1jik%C4%81.xml")
	    (url-unhex-string
	     (or
	      ;; e.g.: "tattvasa%E1%B9%83grahapa%C3%B1jik%C4%81.xml"
	      (substring (cdr (assoc :GET headers)) 1)
	      ""))
	    'utf-8)))
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
			  "TEIwal buffer list")
			 (meta
			  ((charset . "utf-8"))))
		   (body ()
			 (h1
			  ()
			  "TEIwal list of buffers")
			 (ol ()
			     ,(mapconcat
			       (lambda (buf)
				 (teiwal/sxml-to-string
				  (list 'li '()
					(list
					 'a
					 (list
					  (cons 'href (format "/%s" (url-hexify-string buf))))
					 buf)
					(format " <ul><li>file: %s</li><li>size: %s</li></ul>"
						(or
						 (buffer-file-name (get-buffer buf))
						 "[no file]")
						(buffer-size (get-buffer buf)))
					)))
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
;;; add webcomponents first: don't help? 
			 ;; (script
			 ;;  ((src . "/js/webcomp/webcomponentsjs-0.7.22/webcomponents.js"))
			 ;;  "\n// polyfill stuff: https://github.com/webcomponents/webcomponentsjs \n")
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/normalize.css")
			   ;; (charset . "utf-8")
			   ))
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/devanagari.css")
			   ;; (charset . "utf-8")
			   ))
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/CETEIcean.css")
			   (media . "screen")))
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/less.css")))
			 (link
			  ((rel . "stylesheet")
			   (href . "/css/sarit.css")))
			 (meta
			  ((name . "viewport")
			   (content . "width=device-width, initial-scale=1"))
			  "\n")
			 (title
			  ()
			  ,(format "%s (teiwal)" path)))
		   (body () "\n    "
			 (div ((id . "root")) 
			  (nav ((id . "nav")
				(class . "small"))
			       ;; keep content here: avoids <nav/>, which is illegal.
			       "ToC loading, ...")
			  (div
			   ((id . "TEI"))
			   "\n      Trying to load file ... (This page will not work in Internet Explorer and some older browsers. We suggest you use a newer version of Chrome or Firefox.)\n    "))
			 "\n    "
			 (script
			  ((src . "/CETEIcean/dist/CETEI.js"))
			  "\n// See http://teic.github.io/CETEIcean\n")
			 (script
			  ((src . "/js/jquery.js"))
			  "\n // Jquery")
			 (script
			  ((src . "/js/sarit.js"))
			  "\n// SARIT specific extensions \n")
			 (script ()
				 ,(format
				   (concat
				    "jQuery(document).ready(function(){"
				    "saritSetup('buffer/%s');"
				    "});")
				   (url-hexify-string path)))))))
	  (ws-response-header proc 200
			      (cons "Content-type" "text/html"))
	  (ws-send proc (buffer-string))))
       ;; possibly an internal link, check if file exists 
       ((string-match "^buffer/\\(.+\\)$" path)
	(cond
	 ((get-buffer (match-string 1 path))
	  (ws-send proc (with-current-buffer (get-buffer (match-string 1 path))
			  (buffer-string))))
	 ((file-exists-p (expand-file-name (match-string 1 path)))
	  (ws-send-file proc (expand-file-name (match-string 1 path))))
	 (t (ws-response-header proc 404  '("Content-type" . "text/plain")))))
       ((file-exists-p (expand-file-name path "/home/beta/webstuff/emacs-things/teiwal/"))
	(ws-send-file proc (expand-file-name path "/home/beta/webstuff/emacs-things/teiwal/")))
       (t
	(with-current-buffer (get-buffer (teiwal/log-buffer))
	  (save-excursion (goto-char (point-max))
			  (insert (format "Not found: %s" (pp request (current-buffer))))))
	(ws-response-header proc 404  '("Content-type" . "text/plain"))
	(process-send-string proc "Ahem, not found?"))))))

(defun teiwal/template-send-file (proc path &optional mime-type template-vars)
  "Send PATH to PROC.
Optionally explicitly set MIME-TYPE, otherwise it is guessed by
`mm-default-file-encoding'.

An extremely optimistic templating system, ripped off from `ws-send-file'."
  (let ((mime (or mime-type
                  (mm-default-file-encoding path)
                  "application/octet-stream")))))


(defun teiwal/parse-xml (buffer)
  "Abstract XML parsing a bit."
  (with-current-buffer buffer
    (if (featurep 'libxml-parse-xml-region)
	(condition-case nil
	    (libxml-parse-xml-region (point-min) (point-max))
	  (error
	   (condition-case nil
	       (caddr (caddr (libxml-parse-html-region (point-min) (point-max))))
	     (error
	      (car (xml-parse-region (point-min) (point-max)))))))
      (car (xml-parse-region (point-min) (point-max))))))

(defun teiwal/build-nav (buffer)
  "For BUFFER, build a navigation list."
  (let ((xml (teiwal/parse-xml buffer)))
    (when xml
      (teiwal/div-to-list xml))))


(defun teiwal/div-to-list (sxml)
  "Make all div elements in SXML into navigation links.

Often exceeds max-depth, though.  Rewrite with xmltok."
  (cond
   ;; end recursion: empty, string, attributes, or symbol (=tag)
   ((null sxml) '())
   ((stringp sxml) '())
   ;; ignore attributes here (cdr of a consp is a string)
   ((and (listp sxml)
	 (symbolp (car sxml))
	 (stringp (cdr sxml)))
    '())
   ;; keep some
   ((and (symbolp sxml)
	 (member sxml '(head)))
    sxml)
   ;; discard the rest
   ((symbolp sxml) '())
   ;; continue on lists
   ((listp sxml)
    (cons (delq nil (teiwal/div-to-list (car sxml)))
	  (delq nil (teiwal/div-to-list (cdr sxml)))))
   (t (error "Sorry, very unexpected"))))


;;; what to do when called noninteractively
(when noninteractive
  (let (buffers-found)
    (cond
     ((>= 1 (length argv)) (message "Please specify filenames to serve"))
     (t
      (message (format "Opening %s" (cdr argv)))
      (mapcar
       (lambda (filename)
	 (if (file-readable-p (expand-file-name filename))
	     (push (find-file-noselect
		    (expand-file-name filename)
		    'nowarn
		    'raw
		    nil)
		   buffers-found)))
       (cdr argv))
      (message (format "Opened %s" buffers-found))))
    (when buffers-found
      (teiwal/server-start)
      (while (member (process-status (oref teiwal/server-process process)) '(listen connect))
	(message "Process %s okay, sleeping for a while (quit with Ctrl-c)"
		 (oref teiwal/server-process process))
	(sleep-for 3600))))
  ;; avoid processing the commandline args normally
  (setq argv nil))

(provide 'teiwal)
