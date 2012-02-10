;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Error setup tools
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

;; The GPGME library deals with a @emph{lot} of error cases, some of
;; which are moving targets as far as creating bindings are concerned.
;; The procedures in this file are a simple set of tools that allow at
;; least some automation of the error binding process.
;;
;; The libgpg-error distribution includes several files where errors
;; and error sources are defined:
;; 
;; @table @samp
;; @item err-codes.h.in
;;    The GPG-specifice error codes and descriptions.  The format is,
;;    line-by-line,
;;       @code{^[[:digits:]]+[\t]+[A-Z][A-Z_]+[A-Z][\t]+[[:alnum:] ]+$}
;;   
;; @item err-sources.h.in
;;    GPG error sources and descriptions.  The format is the same as
;;    for @samp{err-codes.h.in}
;;
;; @item errnos.in
;;    The system-error translation table. The format is,
;;    line-by-line:
;;       @code{^[[:digit:]]+[\t]+[A-Z]+$}
;;      
;;    A special value, which shows up as
;;       @code{#define GPG_ERR_SYSTEM_ERROR (1 << 15)}
;;    in the resulting @samp{gpg-error.h} file is used as the basis
;;    for the system error translation: the leading number of the
;;    errors declared in @samp{errnos.in} are bitwise-or'ed with
;;    @code{GPG_ERR_SYSTEM_ERROR} to get their final number.  We will
;;    put their actual values in to save on computation time.
;;
;; @end table   
;;
;; At this time these tools are for GPGME/G developers' use only; they
;; require the libgpg-error sources to work properly, which users
;; cannot be expected to have on hand.  As such, the files
;; @samp{error-sources.scm} and @samp{error-codes.scm} are to be
;; distributed complete with each release, or possibly even in the
;; archives themselves.  *** FIXME ***

;;; Code:

(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 pretty-print))


;; Grabbing the info from the libgpg-error source
(define err-regexp
  (make-regexp
   "^([[:digit:]]+)[\t]+([A-Z][A-Z0-9_]+[A-Z0-9])\
[\t]*([[:alnum:] ]+[[:alnum:]])?[[:space:]]*$"))


;; to transform error enum keys to symbols
(define colonize-rx
  ;; get it?  ``colon---ize'' (^_^) (-_-')
  (make-regexp "(GPG_ERR(_SOURCE)?)[_-]"))
(define hyphenize-rx
  (make-regexp "_"))


(define (make-match-list fn rx)
  (let ((p (open-file fn "r")))
    ;; loop for extracting information
    (let loop ((str (read-line p))
	       (lst '()))
      (if (eof-object? str)
	  (begin
	    (close-port p)
	    (reverse lst))
	  (let ((m (regexp-exec rx str)))
	    (loop
	     (read-line p)
	     (if m
		 (cons m lst)
		 lst)))))))

(define (make-gpg-err-symbol c-err)
  (let* ((m (regexp-exec colonize-rx c-err))
	 (type-tag (if (regexp-match? m)
		       (match:substring m 1)
		       #f))
	 (val-tag (if (regexp-match? m)
		      (match:suffix m)
		      c-err)))
    (string->symbol
     (string-downcase
      (regexp-substitute/global
       #f hyphenize-rx
       (string-append
	(if type-tag type-tag "gpg-err")
	":" val-tag)
       'pre "-" 'post)))))


(define (make-error-list match)
  (if (regexp-match? match)
      (let ((err-num (string->number
		      (match:substring match 1)))
	    (err-symbol
	     (make-gpg-err-symbol
	      (match:substring match 2)))
	    (err-desc
	     (match:substring match 3)))
	(list err-symbol err-num err-desc))))

;; The regular expressions used to weed out the cruft in the output

(define prefix-rx (make-regexp "^[[:digit:]]+.*[A-Z], "))

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

;;; err-setup.scm ends here