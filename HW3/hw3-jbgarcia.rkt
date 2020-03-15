#lang racket
(provide (all-defined-out))

;(define pts1 '((0 1) (2 3) (1 0) (3 3) (5 0)))
;(define pts2 '((0 0) (1 1) (2 2) (3 3)))
;(define pts3 '((1 0) (2 1) (3 2) (4 3)))



;Write a function
;computemc that takes as arguments the list of points and returns a list contain-ing two elements:
;the first element is the valuation of m and the second that of c. compute_mc implements the least squares method.

; car = ((x1, y1), (x2,y2), (x3, y3)) = (x1, y1)
; don't have to specify lst(lst), you can write lst, and write code
; that specifies it's a list of list


(define (mean_x lst)
  (/ (compute_x lst) (length lst))
  )

(define (mean_y lst)
  (/(compute_y lst) (length lst))
  )

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))
      )
  )

(define (compute_x lst)
  (if (null? lst)
      0
      (+ (car (car lst)) (compute_x(cdr lst)))
      )
  )

(define (compute_y lst)
  (if(null? lst)
     0
     (+ (cadr (car lst)) (compute_y(cdr lst)))
     )
  )

(define (compute_m_top lst meanx meany)
  (if (null? lst)
      0
      (+
       (*(- (car (car lst)) meanx) (- (cadr (car lst)) meany))
       (compute_m_top (cdr lst) meanx meany))
      ))

(define (compute_m_btm lst meanx)
  (if (null? lst)
      0.0
      (+ (*(- (car (car lst)) meanx) (-(car (car lst)) meanx))      
         (compute_m_btm (cdr lst) meanx)
         )
      ))

(define (get_m lst)
  (if(equal? (compute_m_btm lst (mean_x lst)) 0)
     0.0
     (/ (compute_m_top lst (mean_x lst) (mean_y lst)) (compute_m_btm lst (mean_x lst)))
     ))

(define (get_c lst)
  (-
   (mean_y lst) (* (get_m lst)(mean_x lst)))
  )

;Test why this isn't working
(define (subtract lst meanx )
  (if (null? lst)
      0
      (+ (- (car (car lst)) meanx )
         (subtract (cdr lst) meanx ))
      )
  )

(define (compute_mc lst)
  (list (get_m lst) (get_c lst))
  )

;Returns example '(5 3)
(define (a lst)
  (list (list lst) '() (car lst) 3)
  )



(define (equation_Etest lst m c)
  (if(null? lst)
     0
     (+
      (*(-(cadr (car lst)) (+(* m (car (car lst))) c))
        (-(cadr (car lst)) (+(* m (car (car lst))) c)))
      (equation_Etest(cdr lst) m c) )
     )
  )


(define (de/dm lst m c)
  (if(null? lst)
     0
     (+
      (* (car (car lst)) (- (cadr (car lst)) (+(* m (car (car lst))) c)))
      (de/dm(cdr lst) m c))
     )
  )

(define (de/dc lst m c)
  (if(null? lst)
     0
     (+
      (- (cadr (car lst))  (+(* m (car (car lst))) c))
      (de/dc(cdr lst) m c))
     )
  )


(define (update_m lst L m c)
  (- m (* L (* (/ -2 (length lst)) (de/dm lst m c)))
     ))

(define (update_c lst L m c)
  (- c (* L (* (/ -2 (length lst)) (de/dc lst m c)))
     ))

(define (equation_E lst m c)
  (*(/ 1 (length lst)) (equation_Etest lst m c))
  )


;
;

(define (gradient_mc lst L e cnt)
 
  ( gradient_mcHelper lst L e cnt 0.0 0.0) )

(define (gradient_mcHelper lst L e cnt m c)
  (if(null? lst)
     0
     (if (< (equation_E lst m c) e)
         (list m c (equation_E lst m c))
         (if (equal? cnt 0)
             (list m c (equation_E lst m c))
             (gradient_mcHelper lst L e (- cnt 1) (update_m lst L m c) (update_c lst L m c)) ))
     )
  )
;TESTS

;(define testx (compute_x pts2)) ; Can now just write testx on command line. Have to be after the function you are using though
;(define testy (compute_y pts2))
;(define meanx (mean_x pts2)) ;3/2
;(define compute1 (compute_mc pts1))
;(define compute2 (compute_mc pts2))
;(define compute3 (compute_mc pts3))
;(define mbtm (compute_m_btm pts2 (mean_x pts2)))
;(define mtop (compute_m_top pts2 (mean_x pts2) (mean_y pts2)))
;(define m (get_m list )
;(define sub2 (subtract '((0 1)(1 1)) (mean_x '((0 1 ) ( 1 1)))) ) ;return 0.0

;(define computeG1 (gradient_mc pts1 0.001 0.01 5000))
;(define computeG2 (gradient_mc pts2 0.001 0.01 5000))
;(define computeG3 (gradient_mc pts3 0.001 0.01 5000))
