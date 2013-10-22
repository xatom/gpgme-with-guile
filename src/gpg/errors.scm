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
  #:use-module (srfi srfi-9 gnu)
  #:export     (*default-error-source*
                error-code
                error-code-value
                error-code-description
                error-source
                error-source-value
                error-source-description
                error-object?
                make-error
                system-error->error))

(define gpg-error-lib (dynamic-link "libgpg-error"))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;; Errors
(define-immutable-record-type gpg-error
  (make-gpg-error code source)
  error?
  (code get-error-code)
  (source get-error-source))

(load "error-codes.scm")
(load "error-sources.scm")

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public interface ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the default value for GPGME_ERR_SOURCE_DEFAULT

(define *default-error-source*
  error-source/user-1)


(define (error-object? obj)
  "\
Return @code{#t} if @var{obj} is GPG error object, @code{#f} otherwise."
  (error? obj))


(define (error-code err)
  "\
Return the code object associated with the error @var{err}."
  (get-error-code err))


(define (error-code-value err)
  "\
Return the numeric error code value of the error @var{err}."
  (get-code-value (get-error-code err)))


(define (error-code-description err)
  "\
Return the error code description, a string, for the error @var{err}."
  (get-code-description (get-error-code err)))


(define (error-source err)
  "\
Return the source object associated with the error @var{err}."
  (get-error-source err))


(define (error-source-value err)
  "\
Return the numeric error source value of the error @var{err}."
  (get-source-value (get-error-source err)))


(define (error-source-description err)
  "\
Return the error source description, a string, for the error @var{err}."
  (get-source-description (get-error-source err)))


(define* (make-error code #:optional (source *default-error-source*))
  "\
Return an error object with the value @var{code} from the source
@var{source}.  @code{*default-error-source*} will be used if no
@var{source} is provided.

The default value of @code{*default-error-source*} is
@code{error-source/user-1}.  This can be changed at any time by
@cod{set!}ing @code{*default-error-source*}."
  (cond ((not (error-code? code))
         (scm-error 'gpg-invalid-error-code #f
                    "\
The object ~s is not an error code object"
                    code #f))
        ((not (error-source? source))
         (scm-error 'gpg-invalid-error-source #f
                    "\
The object ~s is not an error source object"
                    source #f))
        (else
         (make-gpg-error code source))))


(define system-error->error
  (let ((gpg-code-from-errno
         (pointer->procedure
          int
          (dynamic-func "gpg_err_code_from_errno" gpg-error-lib)
          (list int))))
   (lambda* (errno #:optional (source *default-error-source*))
     "\
Return an error object translated from the system error value
@var{errno} with the error source @var{source}.  If @var{errno} is not
a recognized GPGME error, @code{error-code/unknown-errno} is used.
@code{*default-error-source*} will be used if no @var{source} is
provided."
     (cond ((not (integer? errno))
            (scm-error 'gpg-invalid-error-number #f
                       "\
The error value must be an integer: ~s" errno #f))
           ((not (error-source? source))
            (scm-error 'gpg-invalid-error-source #f
                       "\
The object ~s is not an error source object"
                       source #f))
           (else
            (let ((err-code (gpg-code-from-errno errno)))
              (make-gpg-error
               (cdr (vhash-assq err-code *error-codes->errors*))
               source)))))))

;;; Error code accessing and translation ;;;
