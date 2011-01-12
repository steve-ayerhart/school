(module lexer mzscheme
  
  (require (lib "class.ss") ; the object system
           (lib "list.ss" "srfi" "1")); for the find and reverse functions
  
  (provide
   token%
   lexer%
   simple-language-lexer
   simple-token-rules
   raise-lexer-error
   exn:fail:lexer-error)
  
  ; subclass the fail exception
  (define-struct (exn:fail:lexer-error exn:fail) ())
  
  ; lexer error exception
  (define (raise-lexer-error p)
    (raise (make-exn:fail:lexer-error
            (string-append "Sorry, Input Error at position: " (number->string p))
            (current-continuation-marks))))
  
  ; token is instantiated with
  ; a token-type and token-value
  (define token%
    (class object% (init init-type
                         init-value)
      (public print get-type get-value)
      
      (field (type init-type)
             (value init-value))
      
      (define (print)
        (begin
          (display "type: ")
          (display type)
          (display ", ")
          (display "value: ")
          (display value)
          (newline)))
      
      (define (get-type)
        type)
      
      (define (get-value)
        value)
      
      (super-new)))
  
  ; lexer is instantiated with a pair of (token-regex . token-type)
  ; the input method takes a string of characters
  (define lexer%
    (class object% (init init-rules)
      
      (public input token tokens print)
      
      (field (rules ; the list of (tokenregex . type) rules
              (map (lambda (rule-type-pair) ; go through the token rules add the case insensitive mode and the ^ metacharacter to the string, convert to pregexp
                     (cons (pregexp (string-append "(?i:" "^" (car rule-type-pair) ")" "[ \t\n\r]*" ))
                           (cdr rule-type-pair)))
                   init-rules))
             (buf "") ; the input program
             (pos 0)) ; current pos of lexer
      
      (define (print)
        (begin
          (display "BUF: ")
          (display buf)
          (newline)
          (display "POS: ")
          (display pos)
          (newline)))
      
      ; sets the buffer
      (define (input str-buf)
        (set! buf str-buf))
      
      ; returns a token
      (define (token)
        (with-handlers ((exn:fail? (lambda (e)
                                     (raise (make-exn:fail:lexer-error
                                             (exn-message e)
                                             (exn-continuation-marks e))))))
          (if (>= pos (string-length buf))
              'DONE
              (begin
                (let ((whitespace? (regexp-match-positions #px"[[:blank:]]*([ \r\n])*" (substring buf pos (string-length buf)))))
                  (if whitespace? ; if we are a space skip it and move on
                      (set! pos (+ pos (cdar whitespace?)))))
                (let ((tokrule (find (lambda (regex-type-pair) ; find the correct token rule from the rule list
                                       (regexp-match? (car regex-type-pair) (substring buf pos (string-length buf))))
                                     rules)))
                  (if tokrule ; if we have a match make a new token, increment pos and return token
                      (let* ((rmp (regexp-match-positions (car tokrule) (substring buf pos (string-length buf))))
                             (token (new token% (init-type (cadr tokrule)) (init-value (substring buf (+ pos (caar rmp)) (+ pos (cdar rmp)))))))
                        (begin
                          (set! pos (+ pos (cdar rmp)))
                          token))
                      ; we didn't find a match raise an error
                      (raise-lexer-error pos)))))))
      
      ; returns a list of tokens
      (define (tokens)
        (let loop-tokens ((lst '()))
          (let ((token-or-done (token)))
            (if (eqv? 'DONE token-or-done)
                (reverse lst)
                (loop-tokens (cons token-or-done lst))))))
      (super-new)))
  
  ; case insensitive
  (define simple-token-rules
    '(("\\d+" NUMBER)
      ("[+]" ADD)
      ("[*]" MULT)
      ("[/]" DIV)
      ("-" SUB)
      ("==" EQ)
      ("!=" NEQ)
      ("<" LT)
      ("<=" LTEQ)
      (">" GT)
      (">=" GTEQ)
      ("input" INPUT)
      ("output" OUTPUT)
      ("set" SET)
      ("if" IF)
      ("while" WHILE)
      ("else" ELSE)
      ("call" CALL)
      ("return" RETURN)
      ("proc" PROC)
      ("," SEPERATOR)
      ("then" THEN)
      ("\\{" LBRACE)
      ("\\}" RBRACE)
      ("do" DO)
      ("endwhile" ENDWHILE)
      ("to" TO)
      ("endif" ENDIF)
      ("return" RETURN)
      ("[a-zA-Z_]+" IDENTIFIER)))
  

  
  (define (simple-language-lexer)
    (new lexer% (init-rules simple-token-rules))))











