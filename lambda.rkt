#lang racket

(define (eval exp env)
  (match exp
    ; function definition
    [`(L ,var ,body)                     `(closure ,exp ,env)]
    ; arithmetic operators
    [`(+ ,a ,b)                          (+ (eval a env) (eval b env))]
    [`(- ,a ,b)                          (- (eval a env) (eval b env))]
    [`(* ,a ,b)                          (* (eval a env) (eval b env))]
    [`(/ ,a ,b)                          (/ (eval a env) (eval b env))]
    [`(% ,a ,b)                          (modulo (eval a env) (eval b env))]
    ; comparison operators
    [`(== ,a ,b)                         (eq? (eval a env) (eval b env))]
    [`(!= ,a ,b)                         (not (eq? (eval a env) (eval b env)))]
    [`(> ,a ,b)                          (> (eval a env) (eval b env))]
    [`(< ,a ,b)                          (< (eval a env) (eval b env))]
    [`(>= ,a ,b)                         (>= (eval a env) (eval b env))]
    [`(<= ,a ,b)                         (<= (eval a env) (eval b env))]
    ; control forms
    [`(let ,var ,val ,body)              (apply (eval `(L ,var ,body) env) val)]
    [`(if ,cond ,then)                   (when (eval cond env) (eval then env))]
    [`(if ,cond ,then ,else)             (if (eval cond env) (eval then env) (eval else env))]
    ; function application
    [`(,f ,e)                            (apply (eval f env) (eval e env))]
    ; atoms
    [(? symbol?)                         (cadr (assq exp env))] ; symbol lookup
    [(? string?)                         exp] ; string
    [(? number?)                         exp] ; number
    [(? boolean?)                        exp] ; boolean
    ))

(define (apply f val)
  (match f
    [`(closure (L ,var ,body) ,env)      (eval body (cons `[,var ,val] env))]))
