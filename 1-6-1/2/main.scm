;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Mon Oct 14 01:43:47 2019
;;
(define-syntax read-number
  (syntax-rules ()
    ((_ nums)
     (define-values nums
       (apply values (map string->number (string-split (read-line) #\space)))))))

(define-syntax read-numbers
  (syntax-rules ()
    ((_ as)
     (define as (map string->number (string-split (read-line) #\space))))
    ((_ as n)
     (define as (map (lambda (_) (map string->number (string-split (read-line) #\space))) (iota n))))))

(define-syntax prlist
  (syntax-rules ()
    ((_ lis)
     (print (string-join (map number->string lis) " ")))))

(define-syntax 1+ (syntax-rules () ((_ x) (+ x 1))))

(define-syntax 1- (syntax-rules () ((_ x) (- x 1))))

;; for gauche 0.9.3.3
(define (append-map thunk l)
  (apply append (map thunk l)))

(define MOD 1000000007)

(read-number (k s))

(define (solve)
  (define ans 0)
  (let loop ((x 0))
    (when (<= x k)
      (let inner ((y 0))
	(when (<= y k)
	  (let1 z (- s x y)
		(when (and (<= 0 z) (<= z k))
		  (set! ans (1+ ans))))
	  (inner (1+ y))))
      (loop (1+ x))))

  (print ans))

(solve)
