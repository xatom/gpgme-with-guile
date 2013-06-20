;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Algorithm handling
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

;; Data encryption uses a number of algorithms, and GPGME/G supports
;; many, both for use with public keys as well as data verification
;; hashes.
;;
;; For the most part, this module does not actually use the underlying
;; GPGME library since the functions and datastructures used by GPGME
;; are very simple; implementing them in Scheme is both simpler and
;; faster than calling to the underlying C library.  Having said that,
;; we provide translation procedures for passing algorithm information
;; to other GPGME functions.

;;; Code:

(define-module (gpg algorithms)
  #:export (pubkey-algo-name
	    hash-algo-name
	    ;; the following are not really intended for user use
	    pubkey-algo->integer
	    integer->pubkey-algo
	    hash-algo->integer
	    integer->hash-algo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public key algorithms ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Data structures

;; A direct translation of the enum gpgme_pubkey_algo_t -> alist.
;; These are the same algorithms exported by libgcrypt
(define *public-key-algorithm-enum*
  '((1   . pk-rsa)
    (2   . pk-rsa-e)
    (3   . pk-rsa-s)
    (16  . pk-elg-e)
    (17  . pk-dsa)
    (20  . pk-elg)
    (301 . pk-ecdsa)
    (302 . pk-ecdh)))


;; Data structure for translating from public key symbol -> int
(define *public-key->integer*
  (map (lambda (p)
	 (and (pair? p)
	      (cons (cdr p) (car p))))
       *public-key-algorithms-enum*))


;; Data structure for translating from public key symbol -> name
(define *public-key->name*
  '((pk-rsa   . "RSA")
    (pk-rsa-e . "RSA-E")
    (pk-rsa-s . "RSA-S")
    (pk-elg-e . "ELG-E")
    (pk-dsa   . "DSA")
    (pk-elg   . "ELG")
    (pk-ecdsa . "ECDSA")
    (pk-ecdh  . "ECDH")))



;;; Public key accessor functions

;; Translate public key symbol -> string
(define (pubkey-algo-name algo)
  "\
Returns a string containing a description of the public key algorithm
@var{algo}.  This string can be used to output the name of the public
key algorithm to the user.

If @var{algo} is not a recognized public key algorithm, returns
@code{#f}."
  (assq-ref *public-key->name* algo))


;; Translate public key symbol -> integer (for interfacing to GPGME)
(define (pubkey-algo->integer algo)
  "\
Translates the public key algorithm symbol @var{algo} to an integer;
this function should only be used if interfacing to GPGME directly.

If @var{algo} is not a recognized public key algorithm, returns
@code{#f}."
  (assq-ref *public-key->integer* algo))


;; Translate integer -> public key symbol (for interfacing to GPGME)
(define (integer->pubkey-algo num)
  "\
Translates the integer @var{num} to its corresponding public key
algorithm symbol; this function should only be used if interfacing
to GPGME directly.

If @var{num} is not a recognized value, returns @code{#f}."
  (assq-ref *public-key-algorithm-enum* num))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash algorithms ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; Data structures

;; A direct translation of gpgme_hash_algo_t -> alist
;; The `md' prefix stands for `message digest'
(define *hash-algorithm-enum*
  '((0 . md-none)
    (1 . md-md5)
    (2 . md-sha1)
    (3 . md-rmd160)
    (5 . md-md2)
    (6 . md-tiger)
    (7 . md-haval)
    (8 . md-sha256)
    (9 . md-sha384)
    (10 . md-sha512)
    (301 . md-md4)
    (302 . md-crc32)
    (303 . md-crc32-rfc1510)
    (304 . md-crc24-rfc2440)))


;; The opposite of *hash-algorithm-enum*
(define *hash->integer*
  (map (lambda (p)
	 (and (pair? p)
	      (cons (cdr p) (car p))))
       *hash-algorithm-enum*))


;; String descriptions of the hash algorithms available
(define *hash->name*
  '((md-md5 . "MD5")
    (md-sha1 . "SHA1")
    (md-rmd160 . "RIPEMD160")
    (md-md2 . "MD2")
    (md-tiger . "TIGER192")
    (md-haval . "HAVAL")
    (md-sha256 . "SHA256")
    (md-sha384 . "SHA384")
    (md-sha512 . "SHA512")
    (md-md4 . "MD4")
    (md-crc32 . "CRC32")
    (md-crc32-rfc1510 . "CRC32RFC1510")
    (md-crc24-rfc2440 . "CRC24RFC2440")))


;;; Hash algorithm accessor functions

;; Translate hash algorithm -> string
(define (hash-algo-name algo)
  "\
Returns a string describing the hash algorithm @var{algo}.  This
string can be used to output the name of the hash algorithm to the
user.

If @var{algo} is not a recognized algorithm, returns @code{#f}."
  (assq-ref *hash->name* algo))


;; Translate hash symbol -> integer (for interfacing to GPGME)
(define (hash-algo->integer algo)
  "\
Translates the hash algorithm symbol @var{algo} to an integer; this
function should only be used if interfacing to GPGME directly.

If @var{algo} is not a recognized hash algorithm, returns @code{#f}."
  (assq-ref *hash->integer* algo))


;; Translate integer -> hash symbol (for interfacing to GPGME)
(define (integer->hash-algo num)
  "\
Translates the integer @var{num} to its corresponding hash algorithm
symbol; this function should only be used if interfacing to GPGME
directly.

If @var{num} is not a recognized value, returns @code{#f}."
  (assq-ref *hash-algorithm-enum* num))

;;; algorithms.scm ends here
