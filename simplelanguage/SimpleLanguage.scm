(module SimpleLanguage mzscheme
	(require scheme/port  
		 (lib "class.ss")
		 (lib "list.ss" "srfi" "1")
		 "lexer.scm"
		 "parser.scm")

	(provide compile-program
		 optimize-compiled-program)


	;(define file-to-compile
	; (command-line
	; #:program "Simplelanguage"
	;#:once-each
	;(("-o" "--optimize") "Compile with optimization"
	;                      (optimize #t))
	;#:args (filename)
	;filename))

	(define (read-program fn)
	  (call-with-input-file fn
				(lambda (port)
				  (let loop ((line (read-line port))
					     (result ""))
				    (if (eof-object? line)
				      result
				      (loop (read-line port) (string-append result line)))))))

	(define (read-all path)
	  (let ((size (file-size path)))
	    (call-with-input-file path
				  (lambda (p)
				    (read-string size p)))))

	(define (compile-program fn)
	  (call-with-output-file (string-append fn "c")
				 (lambda (prt)
				   (let ((test (new parser% (outport prt))))
				     (send test parse (read-all fn))))
				 'replace))

	(define (find-store-loads lst)
	  (let accum ((prev "")
		      (nl ())
		      (lst lst))
	    (if (null? lst)
	      (reverse nl)
	      (let ((cur (regexp-match (pregexp "LOAD (\\w+)") (car lst)))
		    (last (regexp-match (pregexp "STORE (\\w+)") prev)))
		(if (and cur last)
		  (if (string=? (cadr cur)
				(cadr last))
		    (accum (car lst)
			   nl
			   (cdr lst))
		    (accum (car lst)
			   (cons (car lst) nl)
			   (cdr lst)))
		  (accum (car lst)
			 (cons (car lst) nl)
			 (cdr lst)))))))

	(define (remove-bad-stuff lst)
	  (let accum ((lst lst)
		      (nl ()))
	    (if (null? lst)
	      (reverse nl)
	      (accum (cdr lst)
		     (if (not (regexp-match? "[\t]|^$" (car lst)))
		       (cons (car lst) nl)
		       nl)))))

	(define (optimize-compiled-program fn)
	  (call-with-output-file (string-append fn "o")
				 (lambda (prt)
				   (for-each (lambda (s) (display s prt) (newline prt))
					     (call-with-input-file fn
								   (lambda (prt)
								     (find-store-loads (remove-bad-stuff (port->lines prt)))))))
				 'replace)))
