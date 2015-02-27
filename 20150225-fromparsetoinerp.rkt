#lang racket
(require rackunit
         plai)
#|
(define-type AE
  [num (n number?)]
  [plus (lhs AE?)
        (rhs AE?)]
  [minus (lhs AE?)
         (rhs AE?)])
|#

(define-type WAE
  [num (n number?)]
  [plus (lhs WAE?)
        (rhs WAE?)]
  [minus (lhs WAE?)
         (rhs WAE?)]
  [id (sym symbol?)]
  [with (var id?)
        (val WAE?)  ;; The "bound expression"
        (body WAE?) ;; The "body expression"
        ])

(define p1 3)
(define p2 '{+ 3 5})
(define p3 '{with {x 5} {+ 3 x}})
(define p4 '{with {x 5} {with {y 3} {+ x y}}})
(define p5 '{with {x 5} {with {x 3} {+ x 5}}})
(define p6 '{with {z {+ 3 5}} {+ z z}})
(define p7 '{with {z {with {x 4} {+ x x}}} z})


    
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(equal? (first sexp) '+)
     (plus (parse (second sexp))
           (parse (third sexp)))]
    [(equal? (first sexp) '-)
     (minus (parse (second sexp))
            (parse (third sexp)))]
    ;; But what about "with"?
    [(equal? (first sexp) 'with)
     (with (parse (first (second sexp)))
           (parse (second (second sexp)))
           (parse (third sexp)))]
    ;; What happens if we don't handle something?
    [else (error 'parse "Something went wrong")]
    ))

(define (interp ast)
  (type-case WAE ast
    [num (n) n]
    [id (sym) 
        (error 'interp "Unbound identifier: ~a" sym)]
    [plus (lhs rhs)
          (+ (interp lhs)
             (interp rhs))]
    [minus (lhs rhs)
           (- (interp lhs)
              (interp rhs))]
    [with (var val body) 
          (interp (substitute var val body))]
    ))

;; 1. Do not try and process new "with" structs
;;    in the middle of subst.
;; 2. Do not worry about redefinition. Yet.

(define (substitute var val body)
  (type-case WAE body
    [num (n) (num n)]
    [id (sym) 
        (if (equal? var body)
            val
            body)]
    [plus (lhs rhs) 'FIXME]
    [minus (lhs rhs) 'FIXME]
    [with (sub-var sub-val sub-body) 
          (with sub-var sub-val
                'FIXME)]
    ))
       
        
    
    