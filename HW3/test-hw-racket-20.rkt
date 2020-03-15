#lang racket
(require "hw3-jbgarcia.rkt")
; uncomment the lang directive and
; and update the require direvtive with your your own solution.
; Keep the PTS.rkt and your solution and this file in the same folder.
; run this file

(require racket/trace)
(require "PTS.rkt")
(provide (all-defined-out)) 


;; this is to round-up different types of answer-formats
(define (translatelist lst)
  (if (null? lst)
      lst
      (cons (inexact->exact (round (/ (* 10000 (car lst)) 10)))
            (translatelist (cdr lst)))))

;; lists being equal
(define (equallsts? lst1 lst2)
  (if (null? lst1)
      true
      (if (equal? (car lst1) (car lst2))
          (equallsts? (cdr lst1) (cdr lst2))
          false)))





(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))




(define totalpoints 0) ;; total points for this assignment
(define cnt 0)  ;; test counts


(define (utest testcnt testname testfun testpoints)
  (begin
    (write testcnt)
    (write ':)
    (write testname)
    (write ':)
    (write testpoints)
    (writeln 'pts)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (begin
                                   (writeln exn)
                                   (writeln "Exception")
                                   (writeln "incorrect")
                              
                                   ))])
      (if (eval testfun ns)
          (begin
            (writeln "correct")
            (set! totalpoints (+ totalpoints testpoints)))
          (begin
            (writeln "incorrect output")
            
            ))
    )
    ))



(define (hw3)
  (begin

    (writeln '************************************************)
    (writeln 'Tests-on-Q1)
    (writeln '************************************************)


    
    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p1 '(equallsts? (translatelist (compute_mc p1)) '(200 0)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p2 '(equallsts? (translatelist (compute_mc p2)) '(50 100000)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p3 '(equallsts? (translatelist (compute_mc p3)) '(440 0)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p4 '(equallsts? (translatelist (compute_mc p4)) '(50 81980)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p5 '(equallsts? (translatelist (compute_mc p5)) '(200 0)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p6 '(equallsts? (translatelist (compute_mc p6)) '(50 100000)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p7 '(equallsts? (translatelist (compute_mc p7)) '(-5000 20000)) 2)
    
    (writeln '************************************************)
    (writeln 'Tests-on-Q2)
    (writeln '************************************************)
    

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p1 '(equallsts? (translatelist (gradient_mc p1 0.1 0.001 5000)) '(180 48 1)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p2 '(equallsts? (translatelist (gradient_mc p2 0.1 0.001 500)) '(68 99948 1)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p2 '(equallsts? (translatelist (gradient_mc p2 0.1 0.001 50)) '(1489 95897 5643)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p2 '(equallsts? (translatelist (gradient_mc p2  0.1 0.001 5)) '(23150 34604 1435726)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p2 '(equallsts? (translatelist (gradient_mc p2  0.1 0.001 5000)) '(68 99948 1)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p3 '(equallsts? (translatelist (gradient_mc p3 0.1 0.001 5000)) '(440 0 806)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p4 '(equallsts? (translatelist (gradient_mc p4 0.1 0.001 5000)) '(50 81980 1298882)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p5 '(equallsts? (translatelist (gradient_mc p5 0.01 0.0005 5000)) '(187 38 0)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p5 '(equallsts? (translatelist (gradient_mc p5 0.01 0.001 5000)) '(179 52 1)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p6 '(equallsts? (translatelist (gradient_mc p6 0.01 0.0001 5000)) '(56 99983 0)) 2)

    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p3 '(equallsts? (translatelist (gradient_mc p3 0.1 0.001 5000)) '(440 0 806)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p6 '(equallsts? (translatelist (gradient_mc p6 0.01 0.01 5000)) '(110 99828 10)) 2)


    (writeln '-------------)
    (set! cnt (+ cnt 1))
    
    (utest cnt 'p7 '(equallsts? (translatelist (gradient_mc p7 0.01 0.01 1)) '(200 200 142440)) 2)
    

    (writeln '---------------------------------------)
    (write "                      Total Points: ")
    (writeln totalpoints)

    
    )
)

(hw3)

