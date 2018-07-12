#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")

;entry for record; to make the syntax easier
(define-type REC-list
  [rec-entry (name symbol?) (val FWAE?) ]) ;symbol FWAE-num

          
;from lecture 7
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name id?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (params (listof id?)) (body FWAE?)]
  [app (ftn FWAE?) (args (listof FWAE?))] 
  [record (entries (listof rec-entry?))] ;added
  [access (expr FWAE?) (param symbol?)] ;added
  [closureV (params (listof id?)) (body FWAE?) (ds record?)]) ;added
 
;from lecture 7
; num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; lookup : symbol record -> number
; to find a value of parameter in record
(define (lookup name ds)
                         (cond
                           [(empty? (record-entries ds)) (error "free identifier")] 
                           [(equal? (rec-entry-name (first (record-entries ds))) name) (rec-entry-val (first (record-entries ds)))]
                           [else (lookup name (record (rest (record-entries ds))))]))

;;my tests
(test (lookup 'a (record (list (rec-entry 'a (num 10)) (rec-entry 'b (add (num 1) (num 2)))))) (num 10))
(test (lookup 'a (record (list (rec-entry 'a (num 1))))) (num 1))


; arity-check: (listof id) (listof num) -> (listof rec-entry)
; to create the list of rec-entries and check arity
(define (arity-check  l1 l2) (cond
                      [(not (equal? (length l1) (length l2))) (error "wrong arity")]
                      [else (map (lambda (x y) (rec-entry (id-name x) y)) l1 l2)]))

;my tests
(test (arity-check (list (id 'x) (id 'y) (id 'z)) (list (num 1) (num 2) (num 3))) (list (rec-entry 'x (num 1)) (rec-entry 'y (num 2)) (rec-entry 'z (num 3))))
(test (arity-check (list (id 'x) (id 'y) (id 'z)) (list (id 'x) (id 'y) (id 'z))) (list (rec-entry 'x (id 'x)) (rec-entry 'y (id 'y)) (rec-entry 'z (id 'z))))
(test/exn (arity-check (list (id 'x) (id 'y) (id 'z)) (list (id 'x) (id 'y))) "wrong arity")

;interp: FWAE record -> FWAE
;modified to use record as a deferred subst datastructure
(define (interp fwae ds)
  (type-case FWAE fwae
    [num (n) (num n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [with (x i b) (error "not needed")] ;done during parsing
    [id (s) (lookup s ds)]
    [fun (x b) (closureV x b ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (map (lambda (x) (interp x ds)) a))]
                 (interp (closureV-body f-val) 
                         (record (append (arity-check  (closureV-params f-val)  a-val)
                               (record-entries (closureV-ds f-val))))))]
    [record (l) (record (map (lambda (x) (rec-entry (rec-entry-name x) (interp (rec-entry-val x) ds))) l))]
                                           
    [access (r a)  (local [(define entries (record-entries (interp r ds)))]
                       (cond
                         [(empty? entries) (error "no such field")]
                         [(equal? (rec-entry-name (first entries)) a) (rec-entry-val (first entries))]
                         [else (interp (access (record (rest entries)) a) ds)]))]
   [closureV (p b ds) fwae]))

;check-dub-record: list -> boolean
;#t if there are dubs in entry, #f if not
(define (check-dub-record recs)
          (check-duplicates (map (lambda (lst) (first lst)) recs)))

;my tests
(test (check-dub-record (list (list 'x (num 1)) (list 'y (num 2))  (list 'z (num 3)))) #f)
(test (check-dub-record (list (list 'z (num 1)) (list 'y (num 2))  (list 'z (num 3)))) 'z) 
(test (check-dub-record (list (list 'z (num 1)) (list 'y (num 2))  (list 'z (num 3)) (list 'z (num 3)))) 'z) 

; parse-sexpr : sexpr -> FWAE
;; to convert s-expressions into FWAE
(define (parse-sexpr sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (app (fun (list (parse-sexpr x)) (parse-sexpr b)) (list (parse-sexpr i)))]
    [(? symbol?) (id sexp)] 
    [(list 'fun f b) (if (not (check-duplicates (map (lambda (x) (parse-sexpr x)) f))) ;check for duplicates
         (fun (map (lambda (x) (parse-sexpr x)) f) (parse-sexpr b))
         (error "bad syntax"))]

    [(list 'record x ...) (if (not (check-dub-record x)) (record (map (lambda (n) (rec-entry (first n) (parse-sexpr (second n)))) x))
                          (error "duplicate fields"))]
    [(list 'access x y) (access (parse-sexpr x) y)]
    [(list f a ...) (app (parse-sexpr f) (map (lambda (x) (parse-sexpr x)) a))] ;change to map because of multiple args
    [else (error 'parse-sexpr "bad syntax: ~a" sexp)]))

;FWAE->string
;to interpret output of interp
;gives the output based on tests
(define (interp-output fwae)
  (cond
    [(FWAE? fwae)
     (type-case FWAE fwae
       [record (expr) 'record]
       [num (n) n]
       [closureV (p b ds) 'function]
       [else (error "the case is not given")])]
    [else (error "not correct type")]))

;; parses a string containing a FWAE expression to a FWAE AST
(define (parse fwae)
  (parse-sexpr
   (string->sexpr fwae))) 


; run : string -> listof number
;; evaluate a FWAE program contained in a string
(define (run str)
  (interp-output (interp (parse str) (record (list))))) 


;my tests
(test (run "{with {f {fun {} 3}}
                  {with {g {fun {x} {+ x x}}}
                        {with {x 23} {- {+ x {f}} {g 4}}}}}") 18)
(test (run "{record {a 0} {b {+ 2 2}}}") 'record)
(test (run "{access {record {r {record {z 2}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 4}}}} r} z}") 4)
(test (run "{with {x 0} {with {y 9} {access {record {a y} {b x}} a}}}") 9)

;given tests
(test (run "{record {a 10} {b {+ 1 2}}}")
      'record)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}")
      3)
(test/exn (run "{access {record {b 10} {b {+ 1 2}}} b}")
          "duplicate fields")
(test/exn (run "{access {record {a 10}} b}")
          "no such field")
(test (run "{with {g {fun {r} {access r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}")
      12)
(test (run "{access {record {r {record {z 0}}}} r}")
      'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}")
      0)
(test/exn (run "{record {z {access {record {z 0}} y}}}")
          "no such field")
(test (run "{with {f {fun {a b} {+ a b}}}
                  {with {g {fun {x} {- x 5}}}
                        {with {x {f 2 5}} {g x}}}}") 2)
(test (run "{with {f {fun {x y} {+ x y}}} {f 1 2}}") 3)
(test (run "{with {f {fun {} 5}}
                  {+ {f} {f}}}") 10)
(test (run "{with {h {fun {x y z w} {+ x w}}}
                  {h 1 4 5 6}}") 7) 
(test (run "{with {f {fun {} 4}}
                  {with {g {fun {x} {+ x x}}}
                        {with {x 10} {- {+ x {f}} {g 4}}}}}") 6)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}") 3)
(test (run "{with {f {fun {a b} {+ {access a a} b}}}
                  {with {g {fun {x} {+ 5 x}}}
                        {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}") 17)
(test (run "{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}}
                  {access {f 1 2 3 4 5} c}}") 3)
(test (run "{with {f {fun {a b c} {record {a a} {b b} {c c}}}}
                  {access {f 1 2 3} b}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {access {f 1 2 3} y}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {access {f 1 2 3} d}}") 2)
(test (run "{with {f {fun {x} {+ 5 x}}}
                  {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}") 8)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{record {a 10}}") `record)
(test (run "{access {record {a 10}} a}") 10)
(test (run "{access {record {a {+ 1 2}}} a}") 3)
(test (run "{fun {x} x}") 'function)
(test (run "{access {record {a {record {b 10}}}} a}") `record)
(test (run "{access {access {record {a {record {a 10}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{+ {access {record {a 10}} a} {access {record {a 20}} a}}") 30)
(test (run "{+ {access {record {a 10}} a} {access {record {a 20}} a}}") 30)
(test (run "{record {a 10}}") `record)
(test (run "{record {a {- 2 1}}}") `record)
(test (run "{access {record {a 10}} a}") 10)
(test (run "{access {record {a {- 2 1}}} a}") 1)
(test (run "{access {record {a {record {b 10}}}} a}") `record)
(test (run "{access {access {record {a {record {a 10}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {access y y}}") 2)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {access y z}}") 3)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{with {g {fun {r} {access r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}") 12)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)