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



;;; Public key accessor functions

;; Translate public key symbol -> string
(define (gpg:pubkey-algo-name algo)
  "\
Returns a string containing a description of the public key algorithm
@var{algo}.  This string can be used to output the name of the public
key algorithm to the user.

If @var{algo} is not a recognized public key algorithm, returns
@code{#f}."
  (assq-ref *gpg:public-key->name* algo))


;; Translate public key symbol -> integer (for interfacing to GPGME)
(define (gpg:pubkey-algo->integer algo)
  "\
Translates the public key algorithm symbol @var{algo} to an integer;
this function should only be used if interfacing to GPGME directly.

If @var{algo} is not a recognized public key algorithm, returns
@var{#f}."
  (assq-ref *gpg:public-key->integer* algo))


;; Translate integer -> public key symbol (for interfacing to GPGME)
(define (gpg:integer->pubkey-algo num)
  "\
Translates the integer @var{num} to its corresponding public key
algorithm symbol; this function should only be used if interfacing
to GPGME directly.

If @var{num} is not a recognized value, returns @var{#f}."
  (assq-ref *gpg:public-key-algorithm-enum* num))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash algorithms ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; Data structures

;; A direct translation of gpgme_hash_algo_t -> alist
;; The `md' prefix stands for `message digest'
(define *gpg:hash-algorithm-enum*
  '((0 . gpg:md-none)
    (1 . gpg:md-md5)
    (2 . gpg:md-sha1)
    (3 . gpg:md-rmd160)
    (5 . gpg:md-md2)
    (6 . gpg:md-tiger)
    (7 . gpg:md-haval)
    (8 . gpg:md-sha256)
    (9 . gpg:md-sha384)
    (10 . gpg:md-sha512)
    (301 . gpg:md-md4)
    (302 . gpg:md-crc32)
    (303 . gpg:md-crc32-rfc1510)
    (304 . gpg:md-crc24-rfc2440)))


;; The opposite of *gpg:hash-algorithm-enum*
(define *gpg:hash->integer*
  (map (lambda (p)
	 (and (pair? p)
	      (cons (cdr p) (car p))))
       *gpg:hash-algorithm-enum*))


;; String descriptions of the hash algorithms available
(define *gpg:hash->name*
  '((gpg:md-md5 . "MD5")
    (gpg:md-sha1 . "SHA1")
    (gpg:md-rmd160 . "RIPEMD160")
    (gpg:md-md2 . "MD2")
    (gpg:md-tiger . "TIGER192")
    (gpg:md-haval . "HAVAL")
    (gpg:md-sha256 . "SHA256")
    (gpg:md-sha384 . "SHA384")
    (gpg:md-sha512 . "SHA512")
    (gpg:md-md4 . "MD4")
    (gpg:md-crc32 . "CRC32")
    (gpg:md-crc32-rfc1510 . "CRC32RFC1510")
    (gpg:md-crc24-rfc2440 . "CRC24RFC2440")))


;;; Hash algorithm accessor functions

;; Translate hash algorithm -> string
(define (gpg:hash-algo-name algo)
  "\
Returns a string describing the hash algorithm @var{algo}.  This
string can be used to output the name of the hash algorithm to the
user.

If @var{algo} is not a recognized algorithm, returns @code{#f}."
  (assq-ref *gpg:hash->name algo))


;; Translate hash symbol -> integer (for interfacing to GPGME)
(define (gpg:hash-algo->integer algo)
  "\
Translates the hash algorithm symbol @var{algo} to an integer; this
function should only be used if interfacing to GPGME directly.

If @var{algo} is not a recognized hash algorithm, returns @var{#f}."
  (assq-ref *gpg:hash->integer* algo))


;; Translate integer -> hash symbol (for interfacing to GPGME)
(define (gpg:integer->hash-algo num)
  "\
Translates the integer @var{num} to its corresponding hash algorithm
symbol; this function should only be used if interfacing to GPGME
directly.

If @var{num} is not a recognized value, returns @var{#f}."
  (assq-ref *gpg:hash-algorithm-enum* num))

;;; algorithms.scm ends here