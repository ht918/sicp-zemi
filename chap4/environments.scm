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
  (if (= (length variables) (length values))
      (if (null? variables)
          nil
         (cons (cons (car variables) (car values)) (make-frame (cdr variables) (cdr values))))
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



;chap4.1.4
(define (setup-environment)
  (let (( initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment )))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env ))
(define the-global-environment (setup-environment ))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive ))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '> >)
        (list '< <)
        (list '* *)
        (list '- -)
        (list 'display display)
        (list 'load load)))
(define (primitive-procedure-names)
  (map car primitive-procedures ))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc )))
       primitive-procedures ))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let (( input (read )))
    (let (( output (eval input the-global-environment )))
      (announce-output output-prompt)
      (user-print output )))
  (driver-loop ))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline ))
(define (announce-output string)
  (newline) (display string) (newline ))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env> ))
      (display object )))


