;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Algorithm handling
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

;; Data encryption uses a number of algorithms, and GPGME/G supports
;; many, both for use with public keys as well as data verification
;; hashes.
;;
;; For the most part, this module does not actually use the underlying
;; GPGME library since the functions and datastructurens used by GPGME
;; are very simple; implementing them in Scheme is both simpler and
;; faster than calling to the underlying C library.  Having said that,
;; we provide translation procedures for passing algorithm information
;; to other GPGME functions

;;; Code:

(define-module (gpg algorithms)
  #:use-module (system foreign))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public key algorithms ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data structures

;; A direct translation of the enum gpgme_pubkey_algo_t -> alist.
;; These are the same algorithms exported by libgcrypt
(define *gpg:public-key-algorithm-enum*
  '((1   . gpg:pk-rsa)
    (2   . gpg:pk-rsa-e)
    (3   . gpg:pk-rsa-s)
    (16  . gpg:pk-elg-e)
    (17  . gpg:pk-dsa)
    (20  . gpg:pk-elg)
    (301 . gpg:pk-ecdsa)
    (302 . gpg:pk-ecdh)))


;; Data structure for translating from public key symbol -> int
(define *gpg:public-key->integer*
  (map (lambda (p)
	 (and (pair? p)
	      (cons (cdr p) (car p))))
       *gpg:public-key-algorithms-enum*))


;; Data structure for translating from public key symbol -> name
(define *gpg:public-key->name*
  '((gpg:pk-rsa   . "RSA")
    (gpg:pk-rsa-e . "RSA-E")
    (gpg:pk-rsa-s . "RSA-S")
    (gpg:pk-elg-e . "ELG-E")
    (gpg:pk-dsa   . "DSA")
    (gpg:pk-elg   . "ELG")
    (gpg:pk-ecdsa . "ECDSA")
    (gpg:pk-ecdh  . "ECDH")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public key accessor functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Translate public key symbol -> string
(define (gpg:pubkey-algo-name algo)
  "\
Returns a string containing a description of the public key algorithm
@var{algo}.  This string can be used to output the name of the public
key algorithm to the user."
  (assq-ref *gpg:public-key->name* algo))


;; Translate public key symbol -> integer (for interfacing to GPGME)
(define (gpg:pubkey-algo->integer algo)
  "\
Translates the public key algorithm symbol @var{algo} to an integer;
this function should only be used if interfacing to GPGME directly."
  (assq-ref *gpg:public-key->num* algo))


;; Translate integer -> public key symbol (for interfacing to GPGME)
(define (gpg:integer->pubkey-algo num)
  "\
Translates the integer @var{num} to its corresponding public key
algorithm symbol; this function should only be used if interfacing
to GPGME directly."
  (assq-ref *gpg:public-key-algorithm-enum* num))
 
;;; algorithms.scm ends here