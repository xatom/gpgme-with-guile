(use-modules (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex))

;; The regular expressions used in finding the cruft in the output

(define prefix-rx (make-regexp "^[[:digit:]]+.*[A-Z], "))
(define colonize-rx
  ;; get it?  ``colon---ize'' (^_^) (-_-')
  (make-regexp "(ERR)[_-]"))
(define hyphenize-rx
  (make-regexp "_"))
(define error/string-gap-rx
  (make-regexp "\\) =.*\\w, "))
(define trailing-paren-rx
  (make-regexp "\\)$"))

(define (error-string->pair str)
  ;; This will be a cascade of regexp match-and-replace ops
  (call-with-values
      (lambda ()
	(let* ((str-w\o-prefix
		(match:suffix (regexp-exec prefix-rx str)))
	       (gap-match
		(regexp-exec error/string-gap-rx str-w\o-prefix))
	       (err-name
		(match:prefix gap-match))
	       (err-str
		(match:suffix gap-match)))
	  (values
	   ;; first create the error symbol
	   (string->symbol
	    (string-downcase
	     (regexp-substitute/global
	      #f hyphenize-rx
	      (regexp-substitute #f (regexp-exec
				     colonize-rx
				     err-name)
				 'pre 1 ":" 'post)
	      'pre "-" 'post)))

	   ;; Now isolate and return the explanatory string
	   (match:prefix
	    (regexp-exec
	     trailing-paren-rx err-str)))))
    cons))


(define (create-error-code-alist)
  (let* ((output 
	  (open-input-pipe
	   (string-append
	    "gpg-error"
	    (fold (lambda (n s)
		    (string-append
		     " "
		     (number->string n)
		     s))
		  ""
		  (map (lambda (p)
			 (car p))
		       *error-code-alist*)))))
	 (code-strings
	  (let loop ((s (read-line output))
		     (lst '()))
	    (if (eof-object? s) lst
		(loop (read-line output)
		      (cons s lst))))))
    (close-pipe output)
    (map error-string->pair code-strings)))