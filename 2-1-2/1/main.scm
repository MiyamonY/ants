;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Fri Oct 18 03:22:05 2019
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

(define-syntax read-strings
  (syntax-rules ()
    ((_ s)
     (define s (read-line)))
    ((_ ss n)
     (define ss (map (lambda (_) (read-line)) (iota n))))))

(define-syntax prlist
  (syntax-rules ()
    ((_ lis)
     (print (string-join (map number->string lis) " ")))))

(define-syntax 1+ (syntax-rules () ((_ x) (+ x 1))))

(define-syntax 1- (syntax-rules () ((_ x) (- x 1))))

;; for gauche 0.9.3.3
(define (append-map thunk l)
  (apply append (map thunk l)))

(define (vector-index pred vec)
  (define len (vector-length vec))
  (let loop ((i 0))
    (cond ((= i len) #f)
	  ((pred (vector-ref vec i)) i)
	  (else (loop (1+ i))))))

(define MOD 1000000007)

(use util.match)

(read-number (h w))
(read-strings ss h)

(define (aux graph poss)
  (match poss
	 (() #f)
	 (((x . y) . l)
	  (if (and (<= 0 x) (< x h) (<= 0 y) (< y w))
	      (match (vector-ref (vector-ref graph x) y)
		     (#\g #t)
		     (#\# (aux graph l))
		     (else
		      (vector-set! (vector-ref graph x) y #\#)
		      (aux graph (append (list (cons (1- x) y)
					       (cons (1+ x) y)
					       (cons x (1+ y))
					       (cons x (1- y))) l))))
	      (aux graph l)))))

(define (solve)
  (define start
    ((lambda (n) (cons (div n w) (mod n w))) (string-scan (string-join ss "") "s")))
  (define graph (list->vector (map (.$ list->vector string->list) ss)))

  (print (if (aux graph (list start)) "Yes" "No")))

(solve)
