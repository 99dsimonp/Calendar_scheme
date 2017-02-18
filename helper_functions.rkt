; Frequently needed Scheme function definitions from R5RS. You can use (load) the functions in this file
; in case your Scheme system does not support these functions already.

; Portable definitions of 
;   (error error-message-string)
;   (sort-list list leq-cmparison-function) 

; ----------------------------------------------------------------------------------------------------------------
; error - inspirred from http://stackoverflow.com/questions/3120379/drracket-r5rs-and-the-error-procedure.

;; create binding for error
(define error #f)

; capture toplevel continuation
; assign a function to error, allowing a variable number of arguments to be passed
(call-with-current-continuation (lambda (k)
              (set! error
                (lambda error-arguments
                  (display "ERROR: ")
                  (newline)
                  (k error-arguments)))
              'error-function-defined))


; ----------------------------------------------------------------------------------------------------------------
; sort-list

;;; This file is SLIB's sort.scm function by Aubrey Jaffer, agj @ alum.mit.edu,
;;; with elimination of the 'export part' - the last few lines of the original file.
;;;
;;; If you use this file with laml you should load from, or include it in the 
;;; compatibility file and add the definition:
;;; (define sort-list sort:sort)
;;;
;;; "sort.scm" Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;;
;;; This code is in the public domain.

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991
;;; Updated: 19 June 1995

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sort:sorted? seq less?)
    (cond
	((null? seq)
	    #t)
	((vector? seq)
	    (let ((n (vector-length seq)))
		(if (<= n 1)
		    #t
		    (do ((i 1 (+ i 1)))
			((or (= i n)
			     (less? (vector-ref seq i)
				    (vector-ref seq (- i 1))))
			    (= i n)) )) ))
	(else
	    (let loop ((last (car seq)) (next (cdr seq)))
		(or (null? next)
		    (and (not (less? (car next) last))
			 (loop (car next) (cdr next)) )) )) ))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (sort:merge a b less?)
    (cond
	((null? a) b)
	((null? b) a)
	(else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
	    ;; The loop handles the merging of non-empty lists.  It has
	    ;; been written this way to save testing and car/cdring.
	    (if (less? y x)
		(if (null? b)
		    (cons y (cons x a))
		    (cons y (loop x a (car b) (cdr b)) ))
		;; x <= y
		(if (null? a)
		    (cons x (cons y b))
		    (cons x (loop (car a) (cdr a) y b)) )) )) ))


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (sort:merge! a b less?)
    (define (loop r a b)
	(if (less? (car b) (car a))
	    (begin
		(set-cdr! r b)
		(if (null? (cdr b))
		    (set-cdr! b a)
		    (loop b a (cdr b)) ))
	    ;; (car a) <= (car b)
	    (begin
		(set-cdr! r a)
		(if (null? (cdr a))
		    (set-cdr! a b)
		    (loop a (cdr a) b)) )) )
    (cond
	((null? a) b)
	((null? b) a)
	((less? (car b) (car a))
	    (if (null? (cdr b))
		(set-cdr! b a)
		(loop b a (cdr b)))
	    b)
	(else ; (car a) <= (car b)
	    (if (null? (cdr a))
		(set-cdr! a b)
		(loop a (cdr a) b))
	    a)))



;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort:sort! seq less?)
    (define (step n)
	(cond
	    ((> n 2)
		(let* ((j (quotient n 2))
		       (a (step j))
		       (k (- n j))
		       (b (step k)))
		    (sort:merge! a b less?)))
	    ((= n 2)
		(let ((x (car seq))
		      (y (cadr seq))
		      (p seq))
		    (set! seq (cddr seq))
		    (if (less? y x) (begin
			(set-car! p y)
			(set-car! (cdr p) x)))
		    (set-cdr! (cdr p) '())
		    p))
	    ((= n 1)
		(let ((p seq))
		    (set! seq (cdr seq))
		    (set-cdr! p '())
		    p))
	    (else
		'()) ))
    (if (vector? seq)
	(let ((n (vector-length seq))
	      (vec seq))
	  (set! seq (vector->list seq))
	  (do ((p (step n) (cdr p))
	       (i 0 (+ i 1)))
	      ((null? p) vec)
	    (vector-set! vec i (car p)) ))
	;; otherwise, assume it is a list
	(step (length seq)) ))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(define (sort:sort seq less?)
    (if (vector? seq)
	(list->vector (sort:sort! (vector->list seq) less?))
	(sort:sort! (append seq '()) less?)))

; Alias
(define sort-list sort:sort)

(define (filter pred lst)
  (reverse (filter-help pred lst '())))

(define (filter-help pred lst res)
  (cond ((null? lst) res)
        ((pred (car lst)) 
           (filter-help pred (cdr lst)  (cons (car lst) res)))
        (else 
           (filter-help pred (cdr lst)  res))))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (negate p)
  (lambda (x) 
    (if (p x) #f #t)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (find-in-list pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (car lst))
        (else (find-in-list pred (cdr lst)))))

(define (make-selector-function n)
  (lambda (lst) (list-ref lst (- n 1))))

(define (reduce-right f lst)
  (if (null? (cdr lst))
      (car lst)
      (f (car lst) 
         (reduce-right f (cdr lst)))))

(define (reduce-left f lst)
  (reduce-help-left f (cdr lst) (car lst)))

(define (reduce-help-left f lst res)
  (if (null? lst)
      res
      (reduce-help-left f (cdr lst) (f res (car lst)))))

(define (accumulate-right f init lst)
  (if (null? lst)
      init
      (f (car lst) (accumulate-right f init (cdr lst)))))

(define (zip z lst1 lst2)
  (if (null? lst1)
      '()
      (cons 
        (z (car lst1) (car lst2))
        (zip z (cdr lst1) (cdr lst2)))))

(define (curry2 f)
  (lambda(x)
    (lambda(y)
      (f x y))))

(define (curry3 f)
  (lambda(x)
    (lambda(y)
      (lambda(z)
       (f x y z)))))

(define (uncurry2 f)
  (lambda (x y)
    ((f x) y)))

(define (uncurry3 f)
  (lambda (x y z)
    (((f x) y) z)))
