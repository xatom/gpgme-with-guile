;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
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

;; Error handling is an important issue with any library system, but
;; it is of particular concern within GPGME/G since an unhandled error
;; may indeed corrupt some very important (and quite possibly
;; irreplaceable!) data.

;;; Code:

(define-module (gpg errors)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:export     (gpg:error-code->error
		gpg:error->error-code
		gpg:describe-error))

(define gpg-error-lib (dynamic-link "libgpg-error"))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;;; Errors
(define-wrapped-pointer-type
  ;; gpgme_err_code_t
  gpg:error-code
  gpg:error-code?
  gpg:pointer->error-code
  gpg:error-code->pointer
  (lambda (ec p)
    (format p "#<gpg:error-code ~d x~x>"
	    (gpg:error-code->errno ec)
	    (pointer-address (gpg:error-code->pointer ec)))))

(define-wrapped-pointer-type
  ;; gpgme_err_source_t
  gpg:error-source
  gpg:error-source?
  gpg:pointer->error-source
  gpg:error-source->pointer
  (lambda (es p)
    (format p "#<gpg:error-source ~d x~x>"
	    (gpg:error-source-value es)
	    (pointer-address (gpg:error-source->pointer es)))))

(define-wrapped-pointer-type
  ;; gpgme_error_t
  gpg:error
  gpg:error?
  gpg:pointer->error
  gpg:error->pointer
  (lambda (e p)
    (format p "#<gpg:error source: ~a code: ~d x~x>"
	    (gpg:error-source-value (gpg:get-error-source e))
	    (gpg:error-code->errno (gpg:get-error-code e))
	    (pointer-address (gpg:error->pointer e)))))

;; Let's pull in the vhash of error codes.  They are legion!
;; The following variables are provided in @code{error-codes.scm}:
;;
;; @table @samp
;; @item *error-code-alist*
;;   An association list with the numeric error codes as keys and
;;   symbols for each error as the values.
;;
;; @item *gpg-err:code-dim*
;;   The last numeric code allowed, plus one.  Modulo with this to
;;   extract the @emph{real} error codes from GPGME output.
;;
;; @item *gpg:error-codes->errors*
;;   A vhash of the information in @samp{*error-code-alist*}; created
;;   from @samp{*error-code-alist*} at load time to provide faster
;;   look-up times.
;;
;; @item *gpg:errors->error-codes*
;;   A vhash indexed by error symbol rather than error code.  GPGME/G
;;   is designed to use error symbols in the user-facing interface,
;;   dispensing with the C idiom of integers-as-enumerations.  This
;;   structure allows passing error codes back to GPGME if ever
;;   necessary.
;; @end table
(load "error-codes.scm")


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public interface ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg:error-code->error errno)
  "\
Translate the numeric error code @var{errno} to its corresponding
error symbol."
  (cdr (vhash-assq (modulo errno *gpg-err:code-dim*)
		   *gpg:error-codes->errors*)))

(define (gpg:error->error-code err)
  "\
Translate the error symbol @var{err} to its corresponding numeric
error code."
  (cdr (vhash-assq err *gpg:errors->error-codes*)))

(define (gpg:describe-error err)
  "\
Return a string describing the error symbol @var{err}."
  (vhash-assq err *gpg:error-descriptions*))
;;; Error code accessing and translation ;;;
