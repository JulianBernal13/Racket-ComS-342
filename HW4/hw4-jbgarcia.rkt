#lang racket
(require "program.rkt")
(provide (all-defined-out))

;Arithmetic is a list
;Sseq is a list
;CondExpr is a list
;Bcond is a list

;;;; HELPER METHODS

;Returns true if the list size is greater than a number, '() 1, should fail
(define (isEmpty? lst number)
  (if (> (length? lst number) number)
      true;
      false;
      )
  )

; Returns the length of a list
(define (length? lst number)
  (if (null? lst)
      0
      (+ 1 (if (list? lst)
               (length? (cdr lst) number)
               0
               )
         )
      )
  )
; Checks whether the list size is the same as the number T/F
(define (lengthTest? lst number)
  (if (equal? (length? lst number) number)
      true;
      false;
      )
  )


;statement? returns either true or false
(define (synchk in)
  (if (and (list? in) (sseq? in) )
     true;
     false;
)
  )


; (or( (this) (and (this) (this)) ))
(define (sseq? in)
  
   (if (or (and (isEmpty? (car in) 1) (statement? (car in) ))
           (and (isEmpty? (car in) 1) (statement? (car in)) (sseq? (cdr in)) ) )
        (if(isEmpty? (cdr in) 1)
            (sseq? (cdr in))
            true
        )
       false;
      )
   )

(define (statement? in)
  (or (decl? in)
      (assign? in)
      (if? in)
      (while? in)
       )
  )

(define (decl? in)
  (and (list? in) (lengthTest? in 2) (equal? (car in) 'decl) (var? (cadr in))
       )
  )

(define (assign? in)
  (and (list? in) (lengthTest? in 3)
       (equal? (car in) 'assign) (var? (cadr in)) (arithExpr? (caddr in))
       )
  )

(define (if? in)
  (and (list? in) (lengthTest? in 3)
       (equal? (car in) 'if) (condExpr? (cadr in)) (and (list? (cdr(cdr in)) ) (sseq? (caddr in)) )
       )
  )

(define (while? in)
  (and (list? in) (lengthTest? in 3)
       (equal? (car in) 'while) (condExpr? (cadr in)) (and (list? (cdr(cdr in)) ) (sseq? (caddr in)) )
       )
  )

(define (arithExpr? in)
 (if (lengthTest? in 1)
           (or (number? in) (var? in))
           (or (and (list? in) (lengthTest? in 3) (op?(car in)) (arithExpr? (cadr in)) (arithExpr? (caddr in)) ) )
       )
      )

(define (op? in)
  (or (equal? in '+)
      (equal? in '-)
      (equal? in '*)
      (equal? in '/) )
  )

(define (condExpr? in)
  (or (bCond? in)
      (and (list? in) (lengthTest? in 3) (equal? (car in) 'or) (condExpr? (cadr in)) (condExpr? (caddr in)) ) 
      (and (list? in) (lengthTest? in 3) (equal? (car in) 'and) (condExpr? (cadr in)) (condExpr? (caddr in)) )
      (and (list? in) (lengthTest? in 2) (equal? (car in) 'not) (condExpr? (cadr in)) ) 
     )
  )

(define (bCond? in)
  (or (and (lengthTest? in 3) (equal? (car in) 'gt) (arithExpr? (cadr in)) (arithExpr? (caddr in)) )
            (and (lengthTest? in 3) (equal? (car in) 'lt) (arithExpr? (cadr in)) (arithExpr? (caddr in)) ) 
            (and (lengthTest? in 3) (equal? (car in) 'eq) (arithExpr? (cadr in)) (arithExpr? (caddr in)) ) 
         )
  )

(define (var? in)
  (if(symbol? in)
     true;
     false;
     )
  )

;Returns true if the list is greater than a number, '() 1, should fail

(define (yes? in)
 (number? in)
     
  )

;TESTS

;(define lengthT (lengthTest? 'jix 1)) ;Checking if the list size works
;(define lengthF (lengthTest? ' (1 2 3 4) 3)) ;works
;(define test1 (test? 0) ) ; Or with multiple statements works
(define 1st (synchk program1))
;(define 2nnd (synchk 3) ) ; This works (since it fails)
;(define 3nnd (synchk '( (assign x (+ x 1)) )) ) ; This works
;(define y (list? '()))
;(define x (isEmpty? '() 1)) ; This works
;(define 2nd (synchk program2))
;(define 3rd (synchk '((assign x 10) ) )) ; This works
;(define 4th (synchk '(() ))) ; This works (since it fails)

;(define compute2 (compute_mc pts2))
;(define compute3 (compute_mc pts3))


;PROBLEM 2
;;Creates the first block, since the environment CANNOT change in output
(define (sem in env)
  (sem2 in (cons (list 'block) env))
  )

;;Output the environment
(define (sem2 in env)
  (cond
    [ (null? in)   (deleteItem env '(block)) ]
    [ (number? in)   in ]
    [ (equal? in 'block)   in ]
    [ (symbol? in)   in ]
    [ (decl2? (car in)) (sem2 (cdr in) (consEnvDec (car in) env)) ]
    [ (assign2? (car in) env)    (sem2 (cdr in) (consEnvAss (car in) env)) ]
    [ (if2? (car in) env)     (sem2 (cdr in) (consEnvIf (car in) env)) ]
    [ (while2? (car in) env)     (sem2 (cdr in) (consEnvWhile (car in) env)) ]
))
(define (sem3 in env)
  (cond
    [ (null? in) env]
    [ (number? in)   in ]
    [ (equal? in 'block)   in ]
    [ (symbol? in)   in ]
    [ (decl2? (car in)) (sem3 (cdr in) (consEnvDec (car in) env)) ]
    [ (assign2? (car in) env)    (sem3 (cdr in) (consEnvAss (car in) env)) ]
    [ (if2? (car in) env)     (sem3 (cdr in) (consEnvIf (car in) env)) ]
    [ (while2? (car in) env)     (sem3 (cdr in) (consEnvWhile (car in) env)) ]
    
    )
  )

; Looks through the envrironment to see if the variable is there already, returns a number from a variable or a number

(define (decl2? in)
  (and (lengthTest? in 2) (equal? (car in) 'decl) (symbol? (cadr in) )  )
  )

;Adds the path to the environment
(define (consEnvDec varexprpair env)
  (cons (list (cadr varexprpair) 0) env)
  )

; Ex. (assign var Arith)
(define (consEnvAss varexprpair env)
  ;If there's a var before the block, delete it and replace it with new, else just add new
  
  (if(varBeforeFlag? (cadr varexprpair) env)
     (cons (list (cadr varexprpair) (evaluateArithExp (caddr varexprpair) env)) (delete (cadr varexprpair) env env) )
     (changeVar (cadr varexprpair) (evaluateArithExp (caddr varexprpair) env) (list ) env)
  )
  )

; Ex. (if CondExpr (SSeq))
(define (consEnvIf varexprpair env)
  ;If there's a var before the block, delete it and replace it with new, else just add new

  (if(evaluateCondExp (cadr varexprpair) env)
    (deleteUntil '(flag) (sem3 (caddr varexprpair) (cons (list 'flag) env)) )
     env
  )
  )
(define (consEnvWhile varexprpair env)
  ;If there's a var before the block, delete it and replace it with new, else just add new

  (if(evaluateCondExp (cadr varexprpair) env)
        (consEnvWhile varexprpair (deleteUntil '(flag) (sem3 (caddr varexprpair) (cons (list 'flag) env)) ))
        env
  )
  )

;Recursively gets a value from the arith expressions
; Number | Var | (Op ArithExpr ArithExpr)
(define (evaluateArithExp expr env)
  (if(lengthTest? expr 1)
     (if(number? expr)
        expr
        (getVar expr env))
      ( (op2 (car expr))  (evaluateArithExp (cadr expr) env) (evaluateArithExp (caddr expr) env) )     
     )
  )

;Recursively gets a value from the arith expressions
; Bcond | (or CondExpr CondExpr) |(and CondExpr CondExpr) | (not CondExpr)
(define (evaluateCondExp expr env)
  (if (equal? (car expr) 'or)
       (or (evaluateCondExp (cadr expr) env) (evaluateCondExp (caddr expr) env))
       (if (equal? (car expr) 'and)
           (and (evaluateCondExp (cadr expr) env) (evaluateCondExp (caddr expr) env))
           (if (equal? (car expr) 'not)
               (not (evaluateCondExp (cadr expr) env) )
               (evaluateBcondExp? expr env)
               )
           )
 )
 )

(define (evaluateBcondExp? expr env)
  (if(equal? (car expr) 'gt)
     (> (evaluateArithExp (cadr expr) env) (evaluateArithExp (caddr expr) env))
        (if(equal? (car expr) 'lt)
        (< (evaluateArithExp (cadr expr) env) (evaluateArithExp (caddr expr) env))
        (equal? (evaluateArithExp (cadr expr) env) (evaluateArithExp (caddr expr) env)) 
     )
  )
)
;;Get operator
(define (op2 expr)
  (if(equal? expr '+)
     +
     (if(equal? expr '-)
     -
      (if(equal? expr '*)
         *
         /
         )
      )
     )
  )

;Returns the variable with a number
(define (getVar var env)
  (if (equal? var (car (car env)))
      (cadr (car env))
      (getVar var (cdr env))
      )
  )

;Changes a variable within the environment and returns new env
 (define (changeVar var value deletedEnvToAdd env)
   (if(equal? var (car (car env)))
      (append deletedEnvToAdd (cons (list var value) (deleteItem env (car env)) ) )
      (changeVar var value (append deletedEnvToAdd (list (car env))) (cdr env) ) 
      )
   
     )


(define (assign2? in env)
  (and (list? in)
       (equal? (car in) 'assign)
       (symbol? (cadr in))
       (arithExpr2? (caddr in) env)
       )
  )
(define (arithExpr2? in env)
  (if (lengthTest? in 1)
      (or (number? in) (symbol? in))
      (or (and (list? in) (lengthTest? in 3) (op2?(car in)) (arithExpr2? (cadr in) env) (arithExpr2? (caddr in) env) ) ) )
  )

(define (op2? in)
  (or (equal? in '+)
      (equal? in '-)
      (equal? in '*)
      (equal? in '/) )
  )

; (if CondExpr (SSeq))
(define (if2? in env)
  (and (list? in) (lengthTest? in 3)
       (equal? (car in) 'if) (condExpr2? (cadr in) env) (and (list? (cdr(cdr in)) ) (sem2 (caddr in) env) )
       )
  )


(define (while2? in env)
  (and (list? in) (lengthTest? in 3)
       (equal? (car in) 'while) (condExpr2? (cadr in) env) (and (list? (cdr(cdr in)) ) (sem2 (caddr in) env) )
       )
  )

(define (condExpr2? in env)
  (or (bCond2? in env)
      (and (list? in) (lengthTest? in 3) (equal? (car in) 'or) (condExpr2? (cadr in) env) (condExpr2? (caddr in) env) ) 
      (and (list? in) (lengthTest? in 3) (equal? (car in) 'and) (condExpr2? (cadr in) env) (condExpr2? (caddr in) env) )
      (and (list? in) (lengthTest? in 2) (equal? (car in) 'not) (condExpr2? (cadr in) env) ) 
     )
  )

(define (bCond2? in env)
  (or (and (lengthTest? in 3) (equal? (car in) 'gt) (arithExpr2? (cadr in) env) (arithExpr2? (caddr in) env) )
            (and (lengthTest? in 3) (equal? (car in) 'lt) (arithExpr2? (cadr in) env) (arithExpr2? (caddr in) env) ) 
            (and (lengthTest? in 3) (equal? (car in) 'eq) (arithExpr2? (cadr in) env) (arithExpr2? (caddr in) env) ) 
         )
  )




;; removes an object from a list and returns the new list
(define (deleteItem lst item)
  (cond ((null? lst)
         '())
        ((equal? item (car lst))
         (cdr lst))
        (else
         (cons (car lst) 
               (deleteItem (cdr lst) item)))))


;Deletes previous variable (Should just be once), while checking if it's in the same block
(define (delete var curenv env)
  (if (and (equal? var (car (car curenv)) ) (varBeforeFlag? var curenv))
      (deleteItem env (list (car (car curenv)) (cadr (car curenv))) )
      (delete var (cdr curenv) env)
      )
  )

;Deletes all the elements in a list until you see the var (and then you also delete it)
;Return a new env (list)
(define (deleteUntil var env)
  (if (equal? var (car env)) 
      (deleteItem env (car env))
      (deleteUntil var (deleteItem env (car env)))
      )
  )

;Checks to see whether there is a variable inside a nested statement T/F
(define (varBeforeFlag? var env)
  (if (equal? 'flag (car(car env)) )
      false;
      (if (equal? var (car(car env)) )
          true;
          (varBeforeFlag? var (cdr env))
          )
      )
  )

;Reverses a list
(define (reverse1 lst)
  (define (go lst tail)
    (if (null? lst) tail
        (go (cdr lst) (cons (car lst) tail))))
  (go lst '()))

;TESTS:
;(define 1st (sem program3 '()) )

; MAIN TEST CASES WORK
;(define 1st (sem program1 '()) ) ; '((y 10) (x 4)) ;WORKS
;(define 2nd (sem program1 '( (x 20) )) ) ; '((y 10) (x 4) (x 20)) ;WORKS
;(define 3rd (sem program2 '( (y 10) )) ) ; ’((x 11) (z 2) (y 10)) ;WORKS
;(define 4th (sem program2 '( (y 0) )) )  ;'((x 1) (z 0) (y 0)) ;WORKS


;(define 21st (sem program2 '((y 10) )) ) ;’((x 3) (z 1) (y 10)) ;WORKS
;(define 4th (reverse1 '((y 10) (ag rg r)) ) )  ;Works '(())
;(define 3rrd (deleteUntil '(flag) '((y 0) (x 4) (flag) (z 10) )  ))  ;RESULT ; '((z 10)) works
;(define list '((block) (1 3) (1 3) (block) (4 5) (5 1) (block) (block) (block))) ;works
;(define 3rd (deleteItem list '(block)) ) ;Deletes from the front first

;(define program01
;  '((decl x)))
;
;(define a (sem program01 '((x 3)) ) ) ;’((x 0) (x 3) ) ;WORKS