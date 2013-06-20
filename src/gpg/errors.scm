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
  #:export     (error-code->error
		error->error-code
		describe-error))

(define gpg-error-lib (dynamic-link "libgpg-error"))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;;; Errors
(define-wrapped-pointer-type
  ;; gpgme_err_code_t
  error-code
  error-code?
  pointer->error-code
  error-code->pointer
  (lambda (ec p)
    (format p "#<error-code ~d x~x>"
	    (error-code->errno ec)
	    (pointer-address (error-code->pointer ec)))))

(define-wrapped-pointer-type
  ;; gpgme_err_source_t
  error-source
  error-source?
  pointer->error-source
  error-source->pointer
  (lambda (es p)
    (format p "#<error-source ~d x~x>"
	    (error-source-value es)
	    (pointer-address (error-source->pointer es)))))

(define-wrapped-pointer-type
  ;; gpgme_error_t
  error
  error?
  pointer->error
  error->pointer
  (lambda (e p)
    (format p "#<error source: ~a code: ~d x~x>"
	    (error-source-value (get-error-source e))
	    (error-code->errno (get-error-code e))
	    (pointer-address (error->pointer e)))))

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
;; @item *error-codes->errors*
;;   A vhash of the information in @samp{*error-code-alist*}; created
;;   from @samp{*error-code-alist*} at load time to provide faster
;;   look-up times.
;;
;; @item *errors->error-codes*
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

(define (error-code->error errno)
  "\
Translate the numeric error code @var{errno} to its corresponding
error symbol."
  (cdr (vhash-assq (modulo errno *gpg-err:code-dim*)
		   *error-codes->errors*)))

(define (error->error-code err)
  "\
Translate the error symbol @var{err} to its corresponding numeric
error code."
  (cdr (vhash-assq err *errors->error-codes*)))

(define (describe-error err)
  "\
Return a string describing the error symbol @var{err}."
  (vhash-assq err *error-descriptions*))
;;; Error code accessing and translation ;;;
