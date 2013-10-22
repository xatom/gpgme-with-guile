#!/bin/sh

exec guile -e main -s "$0" "$@"
!#

;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Error setup tools
;;;
;;; Copyright © 2011, 2013 Atom X Zane
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
;; At this time these tools are for GPGME/Guile developers' use only;
;; they require the libgpg-error sources to work properly, which users
;; cannot be expected to have on hand.  As such, the files
;; @samp{error-sources.scm} and @samp{error-codes.scm} are to be
;; distributed complete with each release, or possibly even in the
;; archives themselves.  *** FIXME ***

;;; Code:

(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 pretty-print)
             (ice-9 binary-ports)
             (rnrs bytevectors)
	     (srfi srfi-1)
             (system foreign))

(define libgpg-error (dynamic-link "libgpg-error"))

(define *libgpg-error-prefix*
  (let* ((p (open-input-pipe "gpg-error-config --cflags"))
         (dir (substring (read-line p) 2)))
    (close-pipe p)
    dir))

(define error-code->description
  (let ((gpg-strerror
         (pointer->procedure
          '*
          (dynamic-func "gpg_strerror" libgpg-error)
          (list unsigned-int))))
    (lambda (code)
      (pointer->string
       (gpg-strerror code)))))

(define error-source->description
  (let ((gpg-strsource
         (pointer->procedure
          '*
          (dynamic-func "gpg_strsource" libgpg-error)
          (list unsigned-int)))
        (source-mask 127)
        (source-shift (1- (expt 2 7))))
    (lambda (code)
      (pointer->string
       (gpg-strsource (ash (logand code source-mask) 24))))))

(define (error-code->number code)
  (let ((mask (ash 1 15))
        (num
         (string->number
          (string-delete (char-set-complement char-set:digit) code))))
    (if (string-prefix? "GPG_ERR_SYSTEM_ERROR" code)
        (logior mask num)
        num)))

(define (error-string->pair str)
  (let ((err-pair
         (map
          (lambda (s)
            (string-trim-both
             s (char-set #\space #\,)))
          (string-split str #\=))))
    (cons
     (car err-pair)
     (error-code->number (cadr err-pair)))))

(define (c-error-name->symbol cname)
  (define prefix-rx (make-regexp "(GPG_ERR(_SOURCE)?)[_-]"))
  (let ((prefix
         (or (and (string-prefix? "GPG_ERR_SOURCE_" cname)
                  "error-source")
             (and (string-prefix? "GPG_ERR_" cname)
                  "error-code")
             (error "Unknown error symbol" cname)))
        (name (string-join
               (string-split
                (string-downcase
                 (match:suffix
                  (regexp-exec prefix-rx cname)))
                #\_)
               "-")))
    (string->symbol
     (string-append prefix "/" name))))

(define (extract-error-sources error-list)
  (map error-string->pair
       (filter (lambda (str)
                 (and (string-prefix? "GPG_ERR_SOURCE" str)
                      (string-index str #\=)))
               error-list)))

(define (extract-error-codes error-list)
  (map error-string->pair
       (filter (lambda (str)
                 (and (string-prefix? "GPG_ERR" str)
                      (not (string-prefix? "GPG_ERR_SOURCE" str))
                      (string-index str #\=)))
               error-list)))

(define (output-error-code-define error-pair)
  (let ((name (c-error-name->symbol (car error-pair)))
        (value (cdr error-pair))
        (description (error-code->description
                      (cdr error-pair))))
    `(define-public ,name (make-error-code ,value ,description))))

(define (output-error-source-define error-pair)
  (let ((name (c-error-name->symbol (car error-pair)))
        (value (cdr error-pair))
        (description (error-source->description
                      (cdr error-pair))))
    `(define-public ,name (make-error-source ,value ,description))))

(define (extract-error-decls lines)
  (map
   (lambda (line)
     (string-trim-both line (char-set-adjoin char-set:whitespace #\,)))
   (filter (lambda (line)
             (string-match "^[[:space:]]*GPG_ERR_.*[[:digit:]],?" line))
           lines)))

(define (output-error-code-dim input dim-value)
  (regexp-substitute
   #f (string-match "@@ERROR_CODE_DIM@@" input)
   'pre dim-value 'post))

(define (output-error-code-decls input decls)
  (regexp-substitute
   #f (string-match "@@ERROR_CODE_DECLS@@" input)
   'pre decls 'post))

(define (output-error-code-lookup input lookups)
  (let ((l (string-append
            "   `"
            (substring
             (regexp-substitute/global
              #f
              "(error-code/)"
              (with-output-to-string
                (lambda ()
                  (pretty-print
                   (let loop ((rest lookups)
                              (done '()))
                     (if (null? rest)
                         (reverse done)
                         (loop (cdr rest)
                               (if (string= (caar rest) "GPG_ERR_CODE_DIM")
                                   done
                                   (cons
                                    (cons (cdar rest)
                                          (c-error-name->symbol (caar rest)))
                                    done)))))
                   #:per-line-prefix "    ")))
              'pre "," 1 'post)
             4))))
    (regexp-substitute
     #f (string-match "@@ERROR_CODE_LOOKUP@@" input)
     'pre l 'post)))

(define (output-error-source-dim input dim-value)
  (regexp-substitute
   #f (string-match "@@ERROR_SOURCE_DIM@@" input)
   'pre dim-value 'post))

(define (output-error-source-decls input decls)
  (regexp-substitute
   #f (string-match "@@ERROR_SOURCE_DECLS@@" input)
   'pre decls 'post))

(define (output-error-source-lookup input lookups)
  (let ((l (string-append
            "   `"
            (substring
             (regexp-substitute/global
              #f
              "(error-source/)"
              (with-output-to-string
                (lambda ()
                  (pretty-print
                   (let loop ((rest lookups)
                              (done '()))
                     (if (null? rest)
                         (reverse done)
                         (loop (cdr rest)
                               (if (string= (caar rest) "GPG_ERR_SOURCE_DIM")
                                   done
                                   (cons
                                    (cons (cdar rest)
                                          (c-error-name->symbol (caar rest)))
                                    done)))))
                   #:per-line-prefix "    ")))
              'pre "," 1 'post)
             4))))
    (regexp-substitute
     #f (string-match "@@ERROR_SOURCE_LOOKUP@@" input)
     'pre l 'post)))

(define (main . args)
  (let* ((error-decls
          (extract-error-decls
           (let ((p (open-input-file (string-append
                                      *libgpg-error-prefix*
                                      "/gpg-error.h"))))
             (let loop ((line (read-line p))
                        (lines '()))
               (if (eof-object? line)
                   (begin
                     (close-port p)
                     (reverse lines))
                   (loop (read-line p)
                         (cons line lines)))))))
         ;; error codes
         (error-codes (extract-error-codes error-decls))
         (error-code-defines
          (with-output-to-string
            (lambda ()
              (for-each
               (lambda (defn)
                 (if (not (string=
                           (car defn)
                           "GPG_ERR_CODE_DIM"))
                     (pretty-print (output-error-code-define defn))
                     (display "")))
               error-codes))))
         ;; error sources
         (error-sources (extract-error-sources error-decls))
         (error-source-defines
          (with-output-to-string
            (lambda ()
              (for-each
               (lambda (defn)
                 (if (not (string=
                           (car defn)
                           "GPG_ERR_SOURCE_DIM"))
                     (pretty-print (output-error-source-define defn))
                     (display "")))
               error-sources))))
         ;; input files
         (error-codes.scm.in
          (let* ((p (open-input-file
                     "../src/gpg/error-codes.scm.in" #:binary #t))
                 (bv (get-bytevector-all p)))
            (close-port p)
            (utf8->string bv)))
         (error-sources.scm.in
          (let* ((p (open-input-file
                     "../src/gpg/error-sources.scm.in" #:binary #t))
                 (bv (get-bytevector-all p)))
            (close-port p)
            (utf8->string bv)))
         ;; output-files
         (error-codes.scm
          (open-output-file "../src/gpg/error-codes.scm" #:binary #t))
         (error-sources.scm
          (open-output-file "../src/gpg/error-sources.scm" #:binary #t)))
    (put-bytevector
     error-codes.scm
     (string->utf8
      (output-error-code-lookup
       (output-error-code-decls
        (output-error-code-dim
         error-codes.scm.in
         (number->string (assoc-ref error-codes "GPG_ERR_CODE_DIM")))
        error-code-defines)
       error-codes)))
    (close-port error-codes.scm)
    (put-bytevector
     error-sources.scm
     (string->utf8
      (output-error-source-lookup
       (output-error-source-decls
        (output-error-source-dim
         error-sources.scm.in
         (number->string (assoc-ref error-sources "GPG_ERR_SOURCE_DIM")))
        error-source-defines)
       error-sources)))
    (close-port error-sources.scm)))

;; Local Variables:
;; mode: scheme
;; End:
;;; err-setup.scm ends here
