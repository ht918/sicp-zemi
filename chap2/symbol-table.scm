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

(define symbol-table (make-table))

(define (put-symbol symbol id)
  ((symbol-table 'insert!) (list symbol) id))

(define (get-symbol symbol )
  ((symbol-table 'lookup) (list symbol)))

(define (compare-symbol op symbol1 symbol2)
  (op (get-symbol symbol1) (get-symbol symbol2)))