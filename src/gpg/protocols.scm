;;; GPGME/G : GPGME with Guile
;;; 
;;; A Guile binding to the GPGME library
;;; Engines and Protocols
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

;; GPGME soports several cryptographic protocols, but does not
;; @emph{implement} them.  Rather, it relies on backends, or
;; @dfn{engines}, which implement each protocol.  This file defines
;; a binding to GPGME's engines interface.

;;; Code:

(define-module (gpg protocols)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (gpg errors)
  #:use-module (srfi srfi-1)
  #:export (gpg:check-engine-version
	    gpg:get-protocol-name
	    gpg:get-engine-info
	    gpg:set-engine-info
	    gpg:toggle-debug))


(define *gpg:debug* #t)
(define (gpg:toggle-debug)
  (set! *gpg:debug* (not *gpg:debug*)))

;;;;;;;;;;;;;;;;;
;;; Libraries ;;;
;;;;;;;;;;;;;;;;;

(define gpgme-lib (dynamic-link "libgpgme"))
(define gpgme-helper-lib
  (dynamic-link
   (if *gpg:debug*
       "/home/atomx/wd/gpgme-with-guile/src/lib/libguile-gpgme"
       "libguile-gpgme")))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;; Protocols are really just symbols on the Guile-side of this
;; binding, but GPGME defines gpgme_protocol_t as an enum.  Thus,
;; we define an association list of the protocol symbols to numerical
;; values for passing into and receiving out of GPGME functions.
(define *gpg:protocol-enum*
  ;; coincides with gpgme_protocol_t
  '((openpgp  . 0)
    (cms      . 1)
    (gpgconf  . 2)
    (assuan   . 3)
    (g13      . 4)
    (uiserver . 5)
    (default  . 254)
    (unknown  . 255)))

(define *gpg:protocol-lookup-by-number*
  (map (lambda (lst)
	 (cons (cdr lst) (car lst)))
       *gpg:protocol-enum*))

;;; Useful data structures
(define *gpg:protocol-list*
  ;; Association list of accepted encryption protocols
  ;; and their string representations
  '((openpgp  . "OpenPGP")
    (cms      . "CMS")
    (gpgconf  . "GPGCONF")
    (assuan   . "Assuan")
    (g13      . "G13")
    (uiserver . "UIServer")
    (default  . "default")
    (unknown  . "unknown")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         ;;                                          ;;         ;;;
;;;         ;; *** A note about gpgme_engine_info_t *** ;;         ;;;
;;;         ;;                                          ;;         ;;;
;;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         ;;;
;;;                                                                ;;;
;;; The data type `gpgme_engine_info_t' is a pointer to a          ;;;
;;; conventionally packed C struct with the following definition:  ;;;
;;;                                                                ;;;
;;;   struct _gpgme_engine_info                                    ;;;
;;;   {                                                            ;;;
;;;      struct _gpgme_engine_info *next;                          ;;;
;;;      gpgme_protocol_t protocol;                                ;;;
;;;      char *file_name;                                          ;;;
;;;      char *version;                                            ;;;
;;;      const char *req_version;                                  ;;;
;;;      char *home_dir;                                           ;;;
;;;   };                                                           ;;;
;;;   typedef struct _gpgme_engine_info *gpgme_engine_info_t;      ;;;
;;;                                                                ;;;
;;; Normally we'd just treat this as an oqaque pointer and call it ;;;
;;; good, but GPGME doesn't provide any functions for actually     ;;;
;;; _accessing_ the values of this struct.  Therefore, we need to  ;;;
;;; convert to and from a Scheme representation of this structure  ;;;
;;; to smoothly use this information in the most Scheme-like       ;;;
;;; fashion.                                                       ;;;
;;;                                                                ;;;
;;; It appears that it might be easier to develop a small wrapper  ;;;
;;; library for these accesses, but I, your humble binding         ;;;
;;; creator, happen to dislike C greatly, and feel that it would   ;;;
;;; make for a much more enjoyable time if I were to do any        ;;;
;;; conversion via the Scheme side of things.                      ;;;
;;;                                                                ;;;
;;; If _performance_ truly becomes an issue, I _might_ consider    ;;;
;;; wrapping up this code on the C side after all, but the user    ;;;
;;; interface will not change, and at any rate I'm betting against ;;;
;;; that happening, anyhow.                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-wrapped-pointer-type
  ;; this is for passing gpgme_engine_info_t in and out of GPGME
  gpg:engine-info
  gpg:engine-info?
  pointer->gpg:engine-info
  gpg:engine-info->pointer
  (lambda (ei p)
    (format p
      "#<gpg:engine-info @~x>"
      (pointer-address ei))))




;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define (gpg:get-protocol-name gp)
  "\
Returns the protocol name of the protocol @var{gp} as a string,
or @code{#f} if the protocol name is not valid."
  (assq-ref *gpg:protocol-list* gp))

(define gpg:check-engine-version
  (let ((gpg-version (pointer->procedure
		      int
		      (dynamic-func "gpgme_engine_check_version" gpgme-lib)
		      (list int))))
    (lambda (proto)
      "\
Verifies that the engine implementing the protocol @var{proto} is
installed in the expected path and meets the version requirement of
the underlying GPGME library.

This function returns @code{#t} if the engine is available; otherwise
it raises an error with the key @code{gpg-err:inv-engine}."
      (if (zero? (gpg-version (assq-ref *gpg:protocol-enum* proto)))
	  ;; protocols are stored in the alist *gpg:protocol-enum*
	  ;; along with their integer representation, which is used
	  ;; by GPGME
	  #t
	  (scm-error 'gpg-err:inv-engine "gpg:engine-check-version"
		     "The encryption protocol \"~a\" is not available."
		     (list proto) #f)))))


;;; helper functions for gpg:get-engine-info
(define (struct->engine-info-alist st)
  "\
@var{st} is a wrapped `gpg:engine-info' object, which is a hygienic
wrapper for the C struct type `gpgme_engine_info_t.  @var{st} gets
parsed into an alist with the keys `next', `protocol', `file-name',
`version', `required-version', and `home-dir', all of which are
symbols.

Briefly, the values the keys represent are as follows:

@table @samp
@item next
The information for the next engine returned by the GPGME library.
If this is currently the last list of information, the value of
@samp{next} will be the empty list @samp{'()}.

@item protocol
The protocol associated with the engine in question.  At this time it
could be any of the following: @samp{openpgp}; @samp{cms}; 
@samp{gpgconf}; @samp{assuan}; @samp{g13}; @samp{uiserver}; 
@samp{default}; or @samp{unknown}.

@item file-name
The file name of the engine binary.

@item version
The version string of the installed engine.

@item required-version
The minimum version required by the underlying GPGME library.

@item home-dir
The name of the home directory used, or @samp{'()} if it's the
default value.
@end table

Consider this alist immutable; there's no facility provided for the
user to alter these values in the underlying GPGME library."
  (if *gpg:debug*
      (begin
	(display "*** Translating engine info ***\n" (current-error-port))))
  (and (gpg:engine-info? st)
       (let ((lst (parse-c-struct
		   (gpg:engine-info->pointer st)
		   (list '*	      ;pointer to another gpgme_engine_info_t
			 unsigned-int ;represents the gpgme_protocol_t enum
			 '*	      ;string pointer file_name
			 '*           ;string pointer version
			 '*           ;string pointer req_version
			 '*	      ;string pointer home_dir
			 ))))
	 ;; Build up an association list from the retrieved values
	 ;; NOTE: the struct value `next' is a _pointer to another
	 ;; gpgme_engine_info_t struct_.  Therefore, we'll call this
	 ;; function recursively until we hit a null pointer
	 (if *gpg:debug*
	     (begin
	      (display "   Bustin' out an alist!\n" (current-error-port))
	      (display "      The list is: " (current-error-port))
	      (display lst (current-error-port))
	      (newline (current-error-port))))
	 (map (lambda (key val)
		(cons key
		 (case key
		   ((next)
		    (if (eq? val %null-pointer)
			'()
			(struct->engine-info-alist
			 (pointer->gpg:engine-info val))))
		   ((protocol)
		    (assq-ref *gpg:protocol-lookup-by-number* val))
		   ((file-name version required-version home-dir)
		    (if (eq? val %null-pointer)
			'()
			(pointer->string val))))))
	      '(next protocol file-name version required-version home-dir)
	      lst))))

(define (engine-info-alist->struct lst)
  "\
Transform @var{lst} into a C struct for manipulation by the
GPGME library.  Does the opposite of
@code{struct->engine-info-alist}."
  ;; checking for a well-formed association list
  (and (list? lst)
       (fold (lambda (item truth)
	       (and (pair? item) truth))
	     #t
	     lst)
       ;; now we convert to a struct
       (make-c-struct
	(list '* unsigned-int '* '* '* '*)
	;; use `map' to convert alist->struct
	(map (lambda (p)
	       (let ((s (car p))
		     (v (cdr p)))
		 (case s
		   ((next)
		    (if (null? v)
			%null-pointer
			(gpg:engine-info->pointer
			 (engine-info-alist->struct v))))
		   ((protocol)
		    (assq-ref *gpg:protocol-enum* v))
		   ((file-name version required-version home-dir)
		    (if (null? v)
			%null-pointer
			(string->pointer v))))))
	     lst))))


(define gpg:get-engine-info
  (let ((gei (pointer->procedure
	      '*
	      (dynamic-func "retrieve_engine_info" gpgme-helper-lib)
	      (list '*))))
    (if *gpg:debug*
	(display "Attempting to get engine info\n" (current-error-port)))
    (lambda ()
      "\
Create an association list containing the defaults for all protocol
backends available on the system.  If there is an error, returns @code{#f}.

It should be noted that this function is @emph{not} a direct mapping to
@code{gpgme_get_engine_info} for several reasons.  First, a C @code{struct}
really has no place in Scheme code, especially when, in this case, the
@code{struct} is used in an @emph{unsafe}, @emph{imperative} fashion, i.e.
you're expected to access the members of the @code{struct} directly.

To avoid this functionality being exposed to the user, we take care of
all such unsavory practices @emph{behind the scenes}.  This function
provides a much nicer Scheme-like interface to the user, giving all of
the same information that @code{gpgme_get_engine_info} provides, but
in a nice association list, something we're all quite comfortable
with.

Having said that, the keys of the association list returned by this
function are as follows:

@table samp
@item next
The information for the next engine returned by the GPGME library.
If this is currently the last list of information, the value of
@samp{next} will be the empty list @samp{'()}.

@item protocol
The protocol associated with the engine in question.  At this time it
could be any of the following: @samp{openpgp}; @samp{cms}; 
@samp{gpgconf}; @samp{assuan}; @samp{g13}; @samp{uiserver}; 
@samp{default}; or @samp{unknown}.

@item file-name
The file name of the engine binary.

@item version
The version string of the installed engine.

@item required-version
The minimum version required by the underlying GPGME library.

@item home-dir
The name of the home directory used, or @samp{'()} if it's the
default value.
@end table

Consider this alist immutable--engine configuration is handled in a
different fashion by the function @code{gpg:set-engine-info}."
      (call/cc
       (lambda (kont)
	 (let ((result (pointer->gpg:engine-info
			;; we don't do a type check since an error is raised
			;; anyway if we don't have a valid engine object
			(catch #t
			  (lambda ()
			    (if *gpg:debug*
				(display "Trying to retrieve the engine info\n"
					 (current-error-port)))
			    (let ((callback (procedure->pointer
					     '*
					     gpg:error-code->error
					     (list '*))))
			      (if *gpg:debug*
				  (begin
				   (display "Created callback: " (current-error-port))
				   (display callback (current-error-port))
				   (newline (current-error-port))))
			      (gei callback)))
			  (lambda (key routine message . rest)
			    (format (current-error-port)
				    "\
`gpg:get-engine-info' failed with the following key and message:

~3@tKey: ~s
~3@tMessage: ~s\n"
				    key message)
			    (kont #f))))))
	   ;; If we've gotten all the way out here, it's time to make
	   ;; the association list
	   (if *gpg:debug*
	       (display "Got info, now translating.\n" (current-error-port)))
	   (struct->engine-info-alist result)))))))

(define gpg:set-engine-info
  (let ((sei (pointer->procedure
	      unsigned-int
	      (dynamic-func "gpgme_set_engine_info" gpgme-lib)
	      (list unsigned-int '* '*))))
    (lambda* (proto file-name #:optional home-dir)
      "\
Changes the default configuration of the crypto engine implementing
the protocol @var{proto}.

@var{file-name} is the file name of the executable implementing the
protocol; @var{home-dir} is the directory name where the protocol's
configuration can be found.  If @var{home-dir} is not supplied, the
engine's default will be used.

The new defaults for a given engine are not applied to
@code{gpg:context} objects already in use.

If successful, a new association list is returned á la
@code{gpg:get-engine-info}; otherwise an error is thrown with a
key describing the problem."
      (let ((result (sei (assq-ref *gpg:protocol-enum* proto)
			 (string->pointer file-name)
			 (if home-dir
			     (string->pointer home-dir)
			     %null-pointer))))
	(if (zero? result)
	    (gpg:get-engine-info)
	    (scm-error
	     (gpg:error-code->error result)
	     "gpg:set-engine-info"
	     "Unable to set ~a engine information: ~a\n"
	     (list (gpg:describe-error
		    (gpg:error-code->error result)))
	     #f))))))

