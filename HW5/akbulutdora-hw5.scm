(define get-operator (lambda (op)
   (cond
     ( (eq? op '+) +) 
     ( (eq? op '*) *) 
     ( (eq? op '/) /) 
     ( (eq? op '-) -) 
     ( else ((display "cs305: ERROR \n\n") (repl env)))
  )))

(define get-value (lambda (var old-env new-env)
    (cond
      ((null? new-env) (display "cs305: ERROR \n\n") (repl old-env))

      ((equal? (caar new-env) var) (cdar new-env))

      (else (get-value var old-env (cdr new-env))))))

(define extend-env (lambda (var val old-env)
       (cons (cons var val) old-env)))

(define define-stmt? (lambda (e)
    (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)) (expr? (caddr e)))))

(define symbol-in-env? (lambda (e env)
  (and (not (null? env)) (if (eq? (caar env) e) 
      #t 
      (if (> (length env) 1) 
        (symbol-in-env? e (cdr env))
        #f
      )))
))
    
(define expr? (lambda (e)
 (or 
  (number? e)
  (symbol? e)
  (and (list? e) (or (symbol? (car e)) (operation? e) (if-expr? e) (let-expr? e) (lambda? e) (lambda? (car e))))
)))

(define if-expr? 
  (lambda (e)
     (and
       (list? e)
       (= (length e) 4)
       (eq? (car e) 'if)
     )))  

(define operation? (lambda (e)
  (and 
		(list? e)
		(or (eq? '+ (car e)) (eq? '- (car e)) (eq? '* (car e)) (eq? '/ (car e)))
		(> (length e) 2)
	) 
))

(define operation-func(lambda (e env)
  (let (
    (operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
    (operator (get-operator (car e))))
        (apply operator operands)
  ))
)

(define var-binding-list? (lambda (e)
	(and 
		(= (length (car e)) 2)
		(symbol? (caar e))
    ; expr?
		(if (> (length e) 1) (var-binding-list? (cdr e)))
	)
))

(define let-expr? (lambda (e)
     (and
       (= (length e) 3)
       (eq? (car e) 'let)
       (or (eq? (cadr e) '() ) (var-binding-list? (cadr e)))
     )))

(define let-func(lambda (e env)
  (let(
    (vars (map car (cadr e)))
    (vals (map cadr (cadr e)))
    )
    (let 
      ((vals_interpreted (map (lambda (val) (s7-interpret val env)) vals)))
      (let 
        ((new-env (append (map cons vars vals_interpreted) env))) ; bu append yerine recursive kullanabilirim
        (s7-interpret (caddr e) new-env)
      )
    )
  )))

(define formal-binding-list? (lambda (e)
	(and 
    (list? e)
		(symbol? (car e))
		(or (null? (cdr e)) (formal-binding-list? (cdr e)))
	)
))

(define lambda? (lambda (e)
  (and 
    (list? e) 
    (and (eq? 'lambda (car e)) (formal-binding-list? (cadr e)) (expr? (caddr e)) (not (define-stmt? (caddr e)))) 
  )))

(define lambda-func(lambda (e env)
(if (= (length (cadar e)) (length (cdr e)))
											(let* (
                        (parameters (map s7-interpret (cdr e) (make-list (length (cdr e)) env))) 
                        (new-env (append (map cons (cadar e) parameters) env)))
                        (s7-interpret (caddar e) new-env))
											((display "cs305: ERROR \n\n") (repl env)))
))


(define s7-interpret (lambda (e env)
  (if (expr? e)
    (cond
      ((number? e) e)
      ((and (symbol? e) (symbol-in-env? e env)) (get-value e env env))
      ((not (list? e)) (display "cs305: ERROR \n\n") (repl env))
      ((null? e) e)
      ((if-expr? e) (let ((val 
                  (if (not (= (s7-interpret (cadr e) env) 0))
                    (s7-interpret (caddr e) env)
                    (s7-interpret (cadddr e) env)))) val))
      ((let-expr? e) (let-func e env))
      ((lambda? e) e)
      ((lambda? (car e)) (lambda-func e env))
      ((operation? e) (operation-func e env))
      (else (s7-interpret (append (list (get-value (car e) env env)) (cdr e))env ))
      )
    ((display "cs305: ERROR \n\n") (repl env))
    )))


(define repl (lambda (env)
  (let* (
      (dummy1 (display "cs305> "))
      (expr (read))  
      (new-env (if (define-stmt? expr) 
        (extend-env (cadr expr) (s7-interpret (caddr expr) env) env) env)) 
      (val (if (define-stmt? expr)
        (cadr expr)
        (s7-interpret expr env)))
      (dummy2 (display "cs305: "))
      (dummy3 (display val))
      (dummy4 (newline))
      (dummy4 (newline)))
    (repl new-env))))

(define cs305 (lambda () (repl '())))
; (cs305)

;  (define inc (lambda (a) (a+1)))