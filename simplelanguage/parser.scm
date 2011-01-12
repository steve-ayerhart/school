(module parser mzscheme
  (require (lib "class.ss")
           (lib "list.ss" "srfi" "1")
           "lexer.scm")
  
  (provide 
   parser%
   exn:fail:parser-error exn:fail
   raise-parser-error)
  
   ; subclass the fail exception
  (define-struct (exn:fail:parser-error exn:fail) ())
  
  ; lexer error exception
  (define (raise-parser-error reason)
    (raise (make-exn:fail:parser-error
            (string-append "Sorry, Parser error: " reason)
            (current-continuation-marks))))
  
  
  (define parser%
    (class object% (init outport)
      
      (public parse)
      
      (private get-next-token
               match?
               statement
               statement-input
               statement-output
               opnd
               arithop
               relateop
               gen-arithmetic
               gen-condition
               generate-code)

      (field (lexer (new lexer% (init-rules simple-token-rules)))
             (cur-token #f)
             (ifcounter 0)
             (condcounter 0)
             (whilecounter 0)
             (found-main #f)
             (outputport outport))
      
      (define (parse line)
        (begin
          (send lexer input line)
          (get-next-token)
          (program)))
      
      (define (generate-code operation ident)
        (display (string-append operation " " ident "\n") outputport))
      
      (define (generate-label name)
        (display (string-append name "\n") outputport))
      
      
      (define (get-next-token)
        (let ((tok (send lexer token)))
          (if (eqv? 'DONE tok)
              (set! cur-token (new token% (init-type 'DONE) (init-value "DONE")))
              (set! cur-token tok))))
      
      (define (match? type)
        (let ((temp-tok cur-token))
          (if (eqv? (send temp-tok get-type) type)
              (begin
                (get-next-token)
                temp-tok)
              (raise-parser-error "bad token match"))))
      
      (define (new-if-label)
        (set! ifcounter (+ ifcounter 1)))
      
      (define (new-cond-label)
        (set! condcounter (+ condcounter 1)))
      
      (define (new-while-label)
        (set! whilecounter (+ whilecounter 1)))
      
      (define (program)
        (begin
          (generate-code "CALL" "main-proc:")
          (handle-procs)
          (if (not found-main)
              (raise-parser-error "missing main procedure"))))
      
    (define (handle-procs)
      (cond ((eqv? 'PROC (send cur-token get-type))
             (begin
               (match? 'PROC)
               (procedure)))
            ((eqv? 'DONE (send cur-token get-type)) 'DONE)
            (else
             (raise-parser-error "expected a procedure definition"))))
      
      (define (procedure)
        (begin
          (cond ((eqv? 'IDENTIFIER (send cur-token get-type))
                 (let ((nametok (match? 'IDENTIFIER)))
                   (begin
                     (if (regexp-match? (regexp (string-append "(?i:" "main)" "[ \t\n\r]*")) (send nametok get-value))
                         (set! found-main #t))
                     (generate-label (string-append (car (regexp-match (pregexp "\\w+") (send nametok get-value))) "-proc:"))))))
          (statement)))
       
      (define (statement)
        (let ((cur-token-type (send cur-token get-type)))
          (cond ((eqv? 'INPUT cur-token-type)
                 (begin
                   (match? 'INPUT)
                   (statement-input)))
                ((eqv? 'OUTPUT cur-token-type)
                 (begin
                   (match? 'OUTPUT)
                   (statement-output)))
                ((eqv? 'SET cur-token-type)
                 (begin
                   (match? 'SET)
                   (statement-set)))
                ((eqv? 'IF cur-token-type)
                 (begin
                   (match? 'IF)
                   (statement-if)))
                ((eqv? 'WHILE cur-token-type)
                 (begin
                   (match? 'WHILE)
                   (statement-while)))
                ((eqv? 'CALL cur-token-type)
                 (begin
                   (match? 'CALL)
                   (statement-call)))
                ((eqv? 'RETURN cur-token-type)
                 (begin
                   (generate-label "RET")
                   (match? 'RETURN)
                   (handle-procs))))))
        
        (define (statement-call)
          (begin
            (generate-code "CALL" (string-append (car (regexp-match (pregexp "\\w+") (send (match? 'IDENTIFIER) get-value))) "-proc:"))
            (statement)))
                   
   
      (define (statement-input)
        (begin
          (generate-code "IN" (send (match? 'IDENTIFIER) get-value))
          (if (eqv? (send cur-token get-type) 'SEPERATOR)
              (begin
                (match? 'SEPERATOR)
                (statement-input))
              (statement))))
      
      (define (statement-output)
          (begin
            (generate-code "OUT" (send (opnd) get-value))
            (if (eqv? (send cur-token get-type) 'SEPERATOR)
                (begin
                  (match? 'SEPERATOR)
                  (statement-output))
                (statement))))
      
      (define (statement-set)
        (begin
          (let ((ident-tok (match? 'IDENTIFIER)))
            (begin
              ;(generate-code "LOAD" (send ident-tok get-value))
              (match? 'TO)
              (let ((left-tok (opnd))
                    (op-tok (arithop))
                    (right-tok (opnd)))
                (gen-arithmetic op-tok left-tok right-tok))
              (generate-code "STORE" (send ident-tok get-value))))
          (statement)))
      
      (define (statement-if)
        (let ((left-tok (opnd))
              (rel-tok (relateop))
              (right-tok (opnd))
              (locif ifcounter)
              (loccond condcounter))
          (begin
            (new-if-label)
            (new-cond-label)
            (gen-condition loccond left-tok rel-tok right-tok)
            (match? 'THEN)
            (statement)
            (generate-code "J" (string-append "endif" (number->string locif) ":"))
            (generate-label (string-append "cond" (number->string loccond) ":"))
            (if (eqv? 'LBRACE (send cur-token get-type))
                (begin
                  (match? 'LBRACE)
                  (statement)
                  (match? 'RBRACE)))
            (match? 'ENDIF)
            (generate-label (string-append "endif" (number->string locif) ":"))
            (statement))))
      
      (define (statement-while)
        (let ((left-tok (opnd))
              (rel-tok (relateop))
              (right-tok (opnd))
              (locwhile whilecounter)
              (loccond condcounter))
          (begin
            (new-while-label)
            (new-cond-label)
            (generate-label (string-append "dowhile" (number->string locwhile) ":"))
            (gen-condition loccond left-tok rel-tok right-tok)
            (match? 'DO)
            (statement)
            (generate-code "J" (string-append "dowhile" (number->string locwhile) ":"))
            (match? 'ENDWHILE)
            (generate-label (string-append "cond" (number->string loccond) ":"))
            (statement))))
            
      
      (define (gen-arithmetic op opnd1 opnd2)
        (cond ((eqv? (send op get-type) 'ADD)
               (begin
                 (generate-code "LOAD" (send opnd1 get-value))
                 (generate-code "ADD" (send opnd2 get-value))))
              ((eqv? (send op get-type) 'SUB)
               (begin
                 (generate-code "LOAD" (send opnd1 get-value))
                 (generate-code "SUB" (send opnd2 get-value))))
              ((eqv? (send op get-type) 'MULT)
               (begin
                 (generate-code "LOAD" (send opnd1 get-value))
                 (generate-code "MULT" (send opnd2 get-value))))
              ((eqv? (send op get-type) 'DIV)
               (begin
                 (generate-code "LOAD" (send opnd1 get-value))
                 (generate-code "DIV" (send opnd2 get-value))))))
                 
 
      
      (define (gen-condition count opnd1 op opnd2)
        (begin
          (generate-code "LOAD" (send opnd1 get-value))
          (generate-code "SUB" (send opnd2 get-value))
          (generate-code (let ((op-type (send op get-type)))
                           (cond ((eqv? op-type 'EQ)
                                  "JNEQ")
                                 ((eqv? op-type 'NEQ)
                                  "JNEQ")
                                 ((eqv? op-type 'GT)
                                  "JLTEQ")
                                 ((eqv? op-type 'GTEQ)
                                  "JLT")
                                 ((eqv? op-type 'LT)
                                  "JGTEQ")
                                 ((eqv? op-type 'LTEQ)
                                  "JGT")))
                         (string-append "cond" (number->string count) ":"))))
      
      (define (opnd)
        (let ((tok-type (send cur-token get-type)))
          (cond ((eqv? tok-type 'IDENTIFIER)
                 (match? 'IDENTIFIER))
                ((eqv? tok-type 'NUMBER)
                 (match? 'NUMBER)))))
      
      (define (arithop)
        (let ((tok-type (send cur-token get-type)))
          (cond ((eqv? tok-type 'ADD)
                 (match? 'ADD))
                ((eqv? tok-type 'SUB)
                 (match? 'SUB))
                ((eqv? tok-type 'DIV)
                 (match? 'DIV))
                ((eqv? tok-type 'MULT)
                 (match? 'MULT)))))

      (define (relateop)
        (let ((tok-type (send cur-token get-type)))
          (cond ((eqv? tok-type 'EQ)
                 (match? 'EQ))
                ((eqv? tok-type 'NEQ)
                 (match? 'NEQ))
                ((eqv? tok-type 'LT)
                 (match? 'LT))
                ((eqv? tok-type 'LTEQ)
                 (match? 'LTEQ))
                ((eqv? tok-type 'GT)
                 (match? 'GT))
                ((eqv? tok-type 'GTEQ)
                 (match? 'GTEQ)))))
           
          
      (super-new))))