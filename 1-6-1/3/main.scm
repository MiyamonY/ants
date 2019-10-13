;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Mon Oct 14 02:45:31 2019
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

(read-number (n y))

(define (solve)
  (let loop ((a 0))
    (when (<= a n)
      (let loop ((b 0))
	(when (<= (+ a b) n)
	  (let1 c (- n a b)
		(when (= y (+ (* a 10000) (* b 5000) (* c 1000)))
		  (print (format "~s ~s ~s" a b c))
		  (exit)))
	  (loop (1+ b))))
      (loop (1+ a))))

  (print "-1 -1 -1"))

(solve)
