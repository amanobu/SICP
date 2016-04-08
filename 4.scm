(define true #t)
(define false #f)

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond (( primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments ))
        (( compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure ))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure ))))

(define (eval exp env)
  (cond (( self-evaluating? exp) exp)
        (( variable? exp) (lookup-variable-value exp env))
        (( quoted? exp) (text-of-quotation exp))
        (( assignment? exp) (eval-assignment exp env))
        (( definition? exp) (eval-definition exp env))
        ;;;4.4start
        (( and? exp) (eval-and (cdr exp) env))
        (( or? exp) (eval-or exp env))
        ;;;4.4end
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        (( lambda? exp) (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env))
        (( begin? exp)
         (eval-sequence (begin-actions exp) env))
        (( cond? exp) (eval (cond->if exp) env))
        (( application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env )))
        (else
         (error "Unknown expression type: EVAL" exp ))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env ))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env )))

(define (eval-sequence exps env)
  (cond (( last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env ))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;;4.4 start
(define (eval-and exp env)
  (cond ((no-operands? exp) true)
        ((no-operands? (cdr exp)) (eval (car exp) env))
        ((true? (eval (car exp) env)) (eval-and (cdr exp) env))
        (else false)))
;;;  (if (true? (eval (car exp) env))
;;;      (if (no-operands? (cdr exp))
;;;          true
;;;          (eval-and (cdr exp) env)
;;;          )
;;;      false
;;;      )
;;;  )
(define (eval-or exp env)
  (cond ((no-operands? exp) false)
        ((no-operands? (cdr exp)) (eval (car exp) env))
        ((true? (eval (car exp) env)) (eval-and (cdr exp) env))
        (else false)))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
;;;4.4 end


;;;4.6start
;;(let ((var1 (func1)) (var2 (func2))) (body) )
;;(let (( x (+ 2 2))) (+ x x))
(define (let? exp) (tagged-list? exp 'let))
;;;letの変数一覧
(define (let->combination exp)
;; (display (let-params exp))(newline)
;; (display (let-body exp))(newline)
;; (display (let-funcs exp))(newline)
  (display (cons (make-lambda (let-params exp) (let-body exp)) (let-funcs exp)))(newline)
  (cons (make-lambda (let-params exp) (let-body exp)) (let-funcs exp)))

;;;let の変数一覧
(define (let-vars exp)
  (cadr exp))
;;;letのパラメータ
(define (let-params exp)
  (map cadr (let-vars exp)))
;;;letのパラメータに該当する値
(define (let-funcs exp)
  (map cadr (let-vars exp)))
;;;letのbody
(define (let-body exp)
  (cddr exp))
  

;;;4.6end

(define (self-evaluating? exp)
  (cond (( number? exp) true)
        (( string? exp) true)
        (else false )))

(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote ))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false ))
(define (assignment? exp) (tagged-list? exp 'set! ))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define ))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp )))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; 仮引数
                   (cddr exp )))) ; 本体

(define (lambda? exp) (tagged-list? exp 'lambda ))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body )))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp )))
      (cadddr exp)
      'false ))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative ))
(define (begin? exp) (tagged-list? exp 'begin ))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq )))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond (( null? seq) seq)
        (( last-exp? seq) (first-exp seq))
        (else (make-begin seq ))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond ))
;;condの中身(cond (() ()))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else ))
;;;condの条件判定部分
(define (cond-predicate clause) (car clause ))
;;;condのアクション部分
(define (cond-actions clause) (cdr clause ))

;;condのevalから呼び出されるところ
(define (cond->if exp) (expand-clauses (cond-clauses exp )))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; else 節はない
      ;;;firstには最初のcond１つ分。restはその後のもの.elseも含むと思う
      (let (
            (first (car clauses ))
            (rest (cdr clauses ))
            )
        ;;;elseか？
        (if (cond-else-clause? first)
            ;;;その後がnullか→つまりelseがcondの最後にきているか？
            (if (null? rest)
                ;;;その取りbeginに変換
                (sequence->exp (cond-actions first ))
                ;;;文法エラー
                (error "ELSE clause isn't last: COND->IF"
                       clauses ))
            ;;;elseでなければifの形に変換
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first ))
                     (expand-clauses rest ))))))

(define (true? x) (not (eq? x false )))
(define (false? x) (eq? x false ))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure ))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive ))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list 'let let)
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures ))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc )))
       primitive-procedures ))
(define (setup-environment)
  (let (( initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment )))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env ))
(define the-global-environment (setup-environment ))
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
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env> ))
      (display object )))


