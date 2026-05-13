(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.
  
  (require "drscheme-init.scm")
  
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  
  (provide value-of-program value-of)
  
  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  
  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))
  
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp
        
        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))
        
        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))
        
        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))
        
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))
        
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))
        
        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
                 (let ((val1 (value-of exp1 env)))
                   (value-of body
                             (extend-env var val1 env))))
        
        ;do-exp
        (do-exp (ids inits steps bools results)
                (if (null? ids)
                    (eopl:error 'do-exp "do loop with no variables are not allowed")
                    (if (null? bools)
                        (eopl:error 'do-exp "do loop with no booleans are not allowed")
                        (let ((init-env (do-init ids inits env)))
                          (run-do-loop ids steps bools results init-env)))))
        )))
  
  
  ;Helper functions for do-exp
  (define (do-init ids inits env)
    (if (null? ids)
        env
        (let ((val (value-of (car inits) env)))
          (do-init (cdr ids) (cdr inits) (extend-env (car ids) val env)))))
  
  (define (check-do-conditions bools results env)
    (if (null? bools)
        #f 
        (let ((val (value-of (car bools) env)))
          (if (expval->bool val)
              (value-of (car results) env) 
              (check-do-conditions (cdr bools) (cdr results) env)))))
  
  (define (do-steps ids steps env)
    (if (null? ids)
        '()
        (let* ((old-val (apply-env env (car ids)))
               (step-val (value-of (car steps) env))
               (old-num (expval->num old-val))
               (step-num (expval->num step-val))
               (new-num (+ old-num step-num)))
          (cons (num-val new-num) (do-steps (cdr ids) (cdr steps) env)))))
  
  (define (extend-env-list ids vals env)
    (if (null? ids)
        env
        (extend-env (car ids) (car vals) (extend-env-list (cdr ids) (cdr vals) env))))
  
  (define (run-do-loop ids steps bools results env)
    (let ((res (check-do-conditions bools results env)))
      (if res
          res 
          (let ((new-vals (do-steps ids steps env)))
            (run-do-loop ids steps bools results (extend-env-list ids new-vals env)))))))