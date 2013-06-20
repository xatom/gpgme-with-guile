;;; GPGME/Guile : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;;
;;; Copyright © 2011, 2012 Atom X
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

;; This library provides a binding to the GNU Privacy Guard Made Easy
;; (GPGME) library for Guile, the GNU Ubiquitous Intelligent Language
;; for Extension.  GNU Privacy Guard is a system of encryption
;; techniques and utilities designed to protect users' privacy and
;; freedom.
;;
;; It is my hope that others will find this binding useful, and thus
;; spread awareness of privacy issues and educate the average user
;; about how they can protect themselves and others.  By binding GPGME
;; to Guile, including cryptography tools in your average application
;; should become much easier.  If you're already using Guile to
;; provide users with the ability to extend your program, using
;; GPGME is as easy as (use-modules (gpg)). ***FIXME - create modules
;; and structure***

;;; Code:


(define-module (gpg)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (ice-9 i18n)
  #:export (check-version
	    context?))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pull in libraries ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpgme-lib     (dynamic-link "libgpgme-pthread"))
(define gpg-error-lib (dynamic-link "libgpg-error"))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;;; Context
(define-wrapped-pointer-type
  ;; gpgme_ctx_t
  context
  context?
  pointer->context
  context->pointer
  (lambda (c p)
    (format p "#<gpg:context @~x>"
	    (context->pointer c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-version
  (let ((check (pointer->procedure
		'*
		(dynamic-func "gpgme_check_version" gpgme-lib)
		'(*))))
    (lambda version
      "\
Initialize the underlying GPGME library.

@var{version} is not required, but if it is passed to
@code{gpg:check-version} it should be a string of the form \"x.y.z\".

If @var{version} is not passed, @code{gpg:check-version} returns the
version number string of the underlying GPGME library; if @var{version}
is passed, an attempt is made to verify that the underlying library is
either equal to or later than the value indicated, returning the GPGME
version number if successful, or throwing an exception otherwise."
      (if (null? version)
	  (pointer->string (check %null-pointer))
	  (let ((ver (check (string->pointer (car version)))))

	    (if (null-pointer? ver)
		(scm-error 'gpg-version-mismatch
			   #f
			   "\
Underlying GPGME library does not meet version requirement of ~s"
			   (car version) #f)
		(pointer->string ver)))))))


(define set-locale
  (let ((setloc (pointer->procedure
		 '*
		 (dynamic-func "gpgme_set_locale" gpgme-lib)
		 (list '* int '*))))
    (lambda (locale)
      "\
Returns a new @code{gpg:context} whose locale settings are the same as
those of @var{locale}.

@var{locale} is a an object as returned by the @code{make-locale}
function of the @code{(ice-9 i18n)} module shipped with Guile, which
will likewise fulfill the predicate @code{locale?}. ")))


(define make-context
  (let ((gpgme-new (pointer->procedure
		    '*
		    (dynamic-func "gpgme_new" gpgme-lib)
		    (list '*))))
    (lambda ()
      "\
Return a new @code{gpg:context}.

")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the library ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin
  )
