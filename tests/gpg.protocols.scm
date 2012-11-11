;;; GPGME/Guile : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Unit-tests for (gpg protocols)
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

(use-modules (oop goops)
	     (unit-test)
	     (gpg protocols)
	     (srfi srfi-1))

(define (check-engine-info-form engine-info)
  (and (let ((nxt (assq-ref engine-info 'next)))
	 (if (and (list? nxt)
		  (not (null? nxt)))
	     (check-engine-info-form nxt))
	 nxt)
       (assq-ref engine-info 'protocol)
       (assq-ref engine-info 'file-name)
       (assq-ref engine-info 'version)
       (assq-ref engine-info 'required-version)
       (assq-ref engine-info 'home-dir)
       #t))

(define-class <test-gpg-protocols> (<test-case>))

(define-method (test-protocols (self <test-gpg-protocols>))
  (assert-true (string? (gpg:check-engine-version)))
  ;; check our engine info alist for well-formedness
  (let ((ei (gpg:get-engine-info)))
    (assert-true (list? ei))
    (assert-true (check-engine-info-form ei)))

  ;; now attempt to make sure we're reporting
  ;; about the protocols correctly
  
  ;; make sure all of our 
  (assert-true ))

