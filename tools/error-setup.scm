;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Error-extraction tools
;;;
;;; Copyright © 2011 Atom X
;;;
;;; This library is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implide warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this library.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The following is not a part of GPGME-with-Guile proper, but just
;; some handy tools to extract the fault information from GPGME and
;; the gpg-error library.  I make no claims as to the cleanliness nor
;; efficiency of this code.

;;; Code:


(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex))

;; The regular expressions used in finding the cruft in the output

(define prefix-rx (make-regexp "^[[:digit:]]+.*[A-Z], "))
(define colonize-rx
  ;; get it?  ``colon---ize'' (^_^) (-_-')
  (make-regexp "(ERR)[_-]"))
(define hyphenize-rx
  (make-regexp "_"))
(define error/string-gap-rx
  (make-regexp "\\) =.*\\w, "))
(define trailing-paren-rx
  (make-regexp "\\)$"))

(define (error-string->pair str)
  ;; This will be a cascade of regexp match-and-replace ops
  (call-with-values
      (lambda ()
	(let* ((str-w\o-prefix
		(match:suffix (regexp-exec prefix-rx str)))
	       (gap-match
		(regexp-exec error/string-gap-rx str-w\o-prefix))
	       (err-name
		(match:prefix gap-match))
	       (err-str
		(match:suffix gap-match)))
	  (values
	   ;; first create the error symbol
	   (string->symbol
	    (string-downcase
	     (regexp-substitute/global
	      #f hyphenize-rx
	      (regexp-substitute #f (regexp-exec
				     colonize-rx
				     err-name)
				 'pre 1 ":" 'post)
	      'pre "-" 'post)))

	   ;; Now isolate and return the explanatory string
	   (match:prefix
	    (regexp-exec
	     trailing-paren-rx err-str)))))
    cons))


(define (create-error-code-alist)
  (let* ((output 
	  (open-input-pipe
	   (string-append
	    "gpg-error"
	    (fold (lambda (n s)
		    (string-append
		     " "
		     (number->string n)
		     s))
		  ""
		  (map (lambda (p)
			 (car p))
		       *error-code-alist*)))))
	 (code-strings
	  (let loop ((s (read-line output))
		     (lst '()))
	    (if (eof-object? s) lst
		(loop (read-line output)
		      (cons s lst))))))
    (close-pipe output)
    (map error-string->pair code-strings)))

;;; error-setup.scm ends here