;text
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values ))
(define (frame-variables frame) (car frame ))
(define (frame-values frame) (cdr frame ))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame )))
  (set-cdr! frame (cons val (cdr frame ))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals ))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals ))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond (( null? vars)
             (env-loop (enclosing-environment env )))
            ((eq? var (car vars )) (car vals))
            (else (scan (cdr vars) (cdr vals )))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let (( frame (first-frame env )))
          (scan (frame-variables frame)
                (frame-values frame )))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond (( null? vars)
             (env-loop (enclosing-environment env )))
            ((eq? var (car vars )) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals )))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let (( frame (first-frame env )))
          (scan (frame-variables frame)
                (frame-values frame )))))
  (env-loop env))
(define (define-variable! var val env)
  (let (( frame (first-frame env )))
    (define (scan vars vals)
      (cond (( null? vars)
             (add-binding-to-frame! var val frame ))
            ((eq? var (car vars )) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals )))))
    (scan (frame-variables frame) (frame-values frame ))))

;4.11
(define nil '())

(define (make-frame variables values)
  (if (= (length variables values))
      (if (null? variables)
          nil
         (cons (cons (car variables) (car values))) (make-frame (cdr variables) (cdr values)))
      (if (< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values ))))

(define (frame-variables frame)
  (if (null? frame)
      nil
      (cons (caar frame) (frame-variables (cdr frame)))))
(define (frame-values frame)
  (if (null? frame)
      nil
      (cons (cdar frame) (frame-variables (cdr frame)))))

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))



;4.13
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env )))
            ((eq? var (car vars )) (car vals))
            (else (scan (cdr vars) (cdr vals )))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))





