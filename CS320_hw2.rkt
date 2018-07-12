#lang plai

;define WAE
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?)
        (named-expr WAE?)
        (body WAE?)]
  [id (name symbol?)])


;all-free: WAE → list-of-sym
;to list all free identifiers from the given WAE with duplicates and unsorted
(define (all-free wae)
   (type-case WAE wae
    [id (name) (list name)]
    [num (n) '()]
    ;remove all ids bounded by with from body
    [with (name named-expr body) (append (all-free named-expr) (remove* (list name)(all-free body)))]
    [add (lhs rhs) (append (all-free lhs) (all-free rhs))]
    [sub (lhs rhs) (append  (all-free lhs) (all-free rhs))]
    ))


;sorting procedure:
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

; 1)
; free-ids : WAE → list-of-sym
; to find all the free identifiers in the given WAE without dublicates and sort by symbols
(define (free-ids wae)
  (sort (remove-duplicates (all-free wae)) symbol<?))

(test (free-ids (with 'z (num 6) (add (id 'x) (sub (num 3) (id 'x))))) '(x))
(test (free-ids (with 'x (num 1) (sub (id 'z) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b z))
(test (free-ids (add (with 'y (num 0) (with 'y (num 4) (sub (id 'y) (with 'z (num 7) (add (id 'z) (sub (id 'c) (id 'a))))))) (with 'a (id 'd) (id 'a)))) '(a c d))



; all-bind: WAE -> list-of-sym
; to list all binding identifiers from the given WAE with duplicates and unsorted
(define (all-bind wae)
   (type-case WAE wae
    ;add to the list all ids bounded by with
    [with (name named-expr body) (append (list name) (append (all-bind named-expr)(all-bind body)))]
    [num (n) '()]
    [add (lhs rhs) (append (all-bind lhs) (all-bind rhs))]
    [sub (lhs rhs) (append  (all-bind lhs) (all-bind rhs))]
    [id (name) '()]
    ))


; 2)
; binding-ids : WAE → list-of-sym
; to find all the binding identifiers in the given WAE without dublicates and sort by symbols
(define (binding-ids wae)
  (sort (remove-duplicates (all-bind wae)) symbol<?))
(test (binding-ids (with 'x (num 2) (with 'x (id 'x) (add (id 'z) (id 'y))))) '(x))
(test (binding-ids (with 'a (num 2) (with 'a (with 'v (add (num 3) (id 'x)) (sub (id 'a) (id 'b))) (add (id 'z) (id 'x))))) '(a v))
(test (binding-ids (with 'x (num 2) (with 'y (with 'z (add (num 3) (id 'a)) (sub (id 'z) (id 'y))) (with 'y (id 'y) (add (num 7) (id 'w)))))) '(x y z))


; all-bound: WAE -> list-of-sym
; to list all bound identifiers from the given WAE with duplicates and unsorted
(define (all-bound wae)
   (type-case WAE wae
    [with (name named-expr body) (append (cond
                                           [(not (member name (all-free body))) '()]
                                           [else (list name)])
                                         (append (all-bound named-expr)(all-bound body)))]
                                     
    [num (n) '()]
    [add (lhs rhs) (append (all-bound lhs) (all-bound rhs))]
    [sub (lhs rhs) (append  (all-bound lhs) (all-bound rhs))]
    [id (name) '()]
    ))

  
; 3)
; bound-ids : WAE → list-of-sym
; to find all the bound identifiers in the given WAE without dublicates and sort by symbols
(define (bound-ids wae)
  (sort (remove-duplicates
          (all-bound wae)) symbol<?))

(test (bound-ids (with 'a (num 2) (add (id 'x) (with 'b (num 7) (sub (id 'x) (id 'b)))))) '(b))
(test (bound-ids (with 'w (num 2) (with 'y (id 'w) (sub (num 3) (id 'a))))) '(w))
(test (bound-ids (with 'q (num 2) (add (id 'q) (with 'x (id 'x) (sub (num 3) (num 7)))))) '(q))
;; free-ids
(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))

;; binding-ids
(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))

;; bound-ids

(test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
(test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
(test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
(test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
(test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
(test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))


