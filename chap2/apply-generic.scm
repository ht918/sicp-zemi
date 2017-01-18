(define nil '())

(define (make-table)
  (let ((table (list '*table*)))
    (define (lookup keys)
      (define (lookup-iter keys subtable)
        (let ((record (assoc (car keys) (cdr subtable))))
          (if record
              (if (null? (cdr keys))
                  (cdr record)
                  (lookup-iter (cdr keys) record))
              #f)))
      (lookup-iter keys table))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! keys value)
      (define (insert-iter! keys subtable)
        (let ((record (assoc (car keys) (cdr subtable))))
          (if (null? (cdr keys))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons (car keys) value)
                                           (cdr subtable))))
              (if record
                  (insert-iter! (cdr keys) record)
                  (begin
                    (set-cdr! subtable (cons (cons (car keys) nil) (cdr subtable)))
                    (insert-iter! (cdr keys) (cadr subtable)))))))
      (begin
        (insert-iter! keys table)
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'get) table)
            (else (error "Undefined message"))))
    dispatch))

(define op-type-table (make-table))
(define coercion-table (make-table))

(define (put op type item)
  ((op-type-table 'insert!) (list op type) item))
(define (get op type)
  (let ((query
          (if (> (length type) 1)
              (list op  type)
              (list op (car type)))))
      ((op-type-table 'lookup) query)))

(define (put-coercion type1 type2 method)
  ((coercion-table 'insert!) (list type1 type2) method))
(define (get-coercion type1 type2)
  ((coercion-table 'lookup) (list type1 type2)))

(define (attach-tag type-tag contents)
  (cons type-tag contents ))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum )))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum )))

;;ex2.78
;(define (attach-tag type-tag contents)
;  (if (eq? type-tag 'scheme-number)
;      contents
;      (cons type-tag contents )))
;(define (type-tag datum)
;  (if (pair? datum)
;      (car datum)
;      (if (number? datum)
;          'scheme-number
;          (error "Bad tagged datum: TYPE-TAG" datum ))))
;(define (contents datum)
;  (if (pair? datum)
;      (cdr datum)
;      (if (number? datum)
;          datum
;          (error "Bad tagged datum: CONTENTS" datum ))))

(define (apply-generic op . args)
  (let (( type-tags (map type-tag args )))
    (let ((proc (get op type-tags )))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags ))
                    (type2 (cadr type-tags ))
                    (a1 (car args ))
                    (a2 (cadr args )))
                (if (eq? type1 type2)
                    (error "No method for these types1" (list op type-tags ))
                    (let ((t1->t2 (get-coercion type1 type2 ))
                          (t2->t1 (get-coercion type2 type1 )))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types2"
                                         (list op type-tags )))))))
              (error "No method for these types3"
                     (list op type-tags )))))))

(define (install-=zero?)
  (put '=zero? 'rational (lambda (x) (= (car x) 0)))
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
  (put '=zero? 'complex (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put '=zero? 'polynomial (lambda (x) (null? (cdr x)))))

(define (install-equ?)
  (put 'equ? 'scheme-number (lambda (x y) (= x y)))
  (put 'equ? 'rational (lambda (x y) (and (= (car x) (car y))
                                          (= (cdr x) (cdr y)))))
  (put 'equ? 'complex (lambda (x y) (and (= (real-part x) (real-part y))
                                         (= (imag-part x) (imag-part y))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (=zero? n) (apply-generic '=zero? n))
