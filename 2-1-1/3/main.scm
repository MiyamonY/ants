;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Tue Oct 15 00:26:32 2019
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

(use util.match)

(read-number (d g))
(read-numbers as d)

(define (aux score num l unused)
  (if (null? l)
      (cond ((>= score g) num)
	    (else
	     (match-let1 (s p c) (car (append unused '((0 0 0))))
			 (let1 n (ceiling->exact (/ (- g score) s))
			       (if (< n p)
				   (+ num n)
				   1000000000000)))))
      (match-let1 (s p c) (car l)
		  (min (aux (+ score (* p s) c) (+ num p) (cdr l) unused)
		       (aux score num (cdr l) (cons (car l) unused))))))

(define (solve)
  (let1 as (map (lambda (i a) (cons (* 100 i) a)) (iota 10 1) as)
	(print (aux 0 0 as ()))))

(solve)
