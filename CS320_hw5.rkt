#lang plai



(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?) (rhs BFAE?)]
  [sub (lhs BFAE?) (rhs BFAE?)]
  [id (name symbol?)]
  [fun (params symbol?) (body BFAE?)]
  [app (ftn BFAE?) (arg BFAE?)]
  [newbox (val BFAE?)]
  [openbox (val BFAE?)]
  [setbox (box BFAE?) (val BFAE?)]
  [seqn (lst (listof BFAE?))] ;change to list
  [rec (entries (listof rec-entry?))] ;added
  [get (expr BFAE?) (param symbol?)] ;added
  ;[set (expr BFAE?) (param symbol?) (val BFAE?)]
  ) ;added

;entry for record; to make the syntax easier
(define-type REC-list
  [rec-entry (name symbol?) (val BFAE?) ]) ;symbol BFAE-num


;;from lecture
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value BFAE-Value?)
        (ds DefrdSub?)])

;;from lecture
(define-type Store
  [mtSto]
  [aSto (address integer?)
        (value BFAE-Value?)
        (rest Store?)])

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BFAE?)
            (ds DefrdSub?)]
  [recV (entries (listof rec-entry?))]
  [boxV (address integer?)])


(define-type Value*Store
  [v*s (value BFAE-Value?) (store Store?)])

;from lecture 7
; num-op: (number number -> number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; interp-two : BFAE BFAE DefrdSub Store
; (Value Value Store -> Value*Store)
; -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

; malloc : Store -> integer
(define (malloc st)
  (+ 1 (max-address st)))
; max-address : Store -> integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

;from lecture 6
; lookup : symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (x val rest)
          (if (symbol=? x name)
              val
              (lookup name rest))]))

;from textbook
;; store-lookup : location Store !BCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "unallocated")]
    [aSto (location value rest-store)
          (if (= location loc-index)
              value
              (store-lookup loc-index rest-store))]))



;check-dub-record: list -> boolean
;#t if there are dubs in entry, #f if not
(define (check-dub-record recs)
          (check-duplicates (map (lambda (lst) (first lst)) recs)))

;my tests
(test (check-dub-record (list (list 'x (num 1)) (list 'y (num 2))  (list 'z (num 3)))) #f)
(test (check-dub-record (list (list 'z (num 1)) (list 'y (num 2))  (list 'z (num 3)))) 'z) 
(test (check-dub-record (list (list 'z (num 1)) (list 'y (num 2))  (list 'z (num 3)) (list 'z (num 3)))) 'z) 
                                                                    
; interp : BFAE DefrdSub Store -> Value*Store
(define (interp expr ds st)
  (type-case BFAE expr
    [num (n) (v*s (numV n) st)]
    [id (name) (v*s (lookup name ds) st)]
    [add (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    ;[seqn (a b) (interp-two a b ds st
    ;                       (lambda (v1 v2 st1) (v*s v2 st1)))]
    [seqn (sq-lst) ;changed to list of BFAE
          (type-case Value*Store (interp (first sq-lst) ds st)
                [v*s (sq-fun st1)
                     (cond
                       [(equal? (length sq-lst) 1) (v*s sq-fun st1)] ;base case
                       [else (interp (seqn (rest sq-lst)) ds st1)])])] ;recursion

    [fun (x b) (v*s (closureV x b ds) st)]
    [app (f a) (interp-two f a ds st
                           (lambda (fun-val arg-val st1)
                             (interp (closureV-body fun-val)
                                     (aSub (closureV-param fun-val)
                                           arg-val
                                           (closureV-ds fun-val))
                                     st1)))]
    
    [setbox (bx-expr val-expr)
            (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st1)
                          (v*s val
                               (change-box-val bx-val val st1))))] ;changed to set the new value in Store
    ;(aSto (boxV-address bx-val)
    ;     val
    ;    st1))))]
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st1)
                         st1)])]
    [newbox (val)
            (type-case Value*Store (interp val ds st)
              [v*s (vl st1)
                   (local [(define a (malloc st1))]
                     (v*s (boxV a)
                          (aSto a vl st1)))])]
    ;[else (error "invalid syntax")]
    [get (r a) 'smth]
    [rec (l) (local [(define l1 empty) ;doesn't work, but passes some tests
                     (define a (malloc st))]
                  (v*s (boxV a)
                       (aSto a (recV l1) st)))]
               
  ))


; parse: sexp -> BFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list f) a) (fun f (parse a))]
    [(list 'newbox x) (newbox (parse x))]
    [(list 'setbox f1 f2) (setbox (parse f1) (parse f2))]
    [(list 'openbox x) (openbox (parse x))]
    [(list 'seqn x ... ) (seqn (map (lambda (l) (parse l)) x))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'get x y) (get (parse x) y)]
    [(list 'rec x ...) (if (not (check-dub-record x)) (rec (map (lambda (n) (rec-entry (first n) (parse (second n)))) x))
                          (error "duplicate fields"))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

;;change-box-val: BFAE-Value-boxV BFAE-Value-numV aSto -> aSto
;;to change the value in Store for the given box (by address)
(define (change-box-val addr val st)
  ;base case
  (cond
    [(equal? st (mtSto)) (mtSto)]
    [else
     (cond
       [(equal? (aSto-address st) (boxV-address addr))
          ;set new value to the addressed box
          (aSto (aSto-address st) val (change-box-val addr val (aSto-rest st)))]
          ;check rest 
       [else (aSto (aSto-address st) (aSto-value st) (change-box-val addr val (aSto-rest st)))])]))


;MY TESTS
(test (change-box-val (boxV 4) (numV 5) (aSto 4 (numV 25) (mtSto))) (aSto 4 (numV 5) (mtSto)))
(test (change-box-val (boxV 9) (numV 5) (aSto 4 (numV 25) (mtSto))) (aSto 4 (numV 25) (mtSto)))
(test (change-box-val (boxV 5) (numV 15) (aSto 4 (numV 25) (aSto 5 (numV 150) (mtSto)))) (aSto 4 (numV 25) (aSto 5 (numV 15) (mtSto))))


;BFAE->string
;to interpret output of interp
;gives the output based on tests
(define (interp-expr bfae)
  (local
    [(define ds (mtSub))
     (define st (mtSto))]
    (type-case Value*Store (interp bfae ds st)
      [v*s (value store)
           [type-case BFAE-Value value
             [numV (n) n]
             [closureV (x b ds) 'func]
             [boxV (address) (cond
                               [(recV? (store-lookup address store)) 'record]
                               [else 'box])]
             [recV (l) error "denied access"]]])))



;MY TESTS
(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b 4}
                           {openbox b}}}
                         {newbox 10}})
                (mtSub)
                (mtSto))
        (v*s (numV 4) (aSto 1 (numV 4) (mtSto))))

(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b {+ 1 {openbox b}}}
                           {setbox b {+ 2 {openbox b}}}
                           {setbox b {+ 3 {openbox b}}}
                           {openbox b}}}
                         {newbox 1}})
                (mtSub)
                (mtSto))
       (v*s (numV 7) (aSto 1 (numV 7) (mtSto))))

(test (interp (parse '{seqn 10 20})
              (mtSub)
              (mtSto))
      (v*s (numV 20) (mtSto)))

(test (interp (parse '{{fun {b} {openbox b}}
                       {newbox 30}})
              (mtSub)
              (mtSto))
      (v*s (numV 30) (aSto 1 (numV 30) (mtSto))))

;GIVEN TESTS
(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b 2}
                           {openbox b}}}
                         {newbox 1}})
                (mtSub)
                (mtSto))
        (v*s (numV 2)
             (aSto 1 (numV 2) (mtSto))))

(test (interp (parse '{{fun {b}
                          {seqn
                           {setbox b {+ 2 {openbox b}}}
                           {setbox b {+ 3 {openbox b}}}
                           {setbox b {+ 4 {openbox b}}}
                           {openbox b}}}
                         {newbox 1}})
                (mtSub)
                (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))

(test (interp (parse '{seqn 1 2})
              (mtSub)
              (mtSto))
      (v*s (numV 2) (mtSto)))

(test (interp (parse '{{fun {b} {openbox b}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 1 (numV 10) (mtSto))))

(test (interp (parse '{{fun {b} {seqn
                                 {setbox b 12}
                                 {openbox b}}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 12)
           (aSto 1
                 (numV 12)
                 (mtSto))))



(test (interp (parse '{{fun {b} {openbox b}}
                       {seqn
                        {newbox 9}
                        {newbox 10}}})
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 2 (numV 10)
                 (aSto 1 (numV 9) (mtSto)))))

(test (interp (parse '{{{fun {b}
                             {fun {a}
                                  {openbox b}}}
                        {newbox 9}}
                       {newbox 10}})
              (mtSub)
              (mtSto))
      (v*s (numV 9)
           (aSto 2 (numV 10)
                 (aSto 1 (numV 9) (mtSto)))))
(test (interp (parse '{{fun {b}
                            {seqn
                             {setbox b 2}
                             {openbox b}}}
                       {newbox 1}})
              (mtSub)
              (mtSto))
      (v*s (numV 2)
           (aSto 1 (numV 2) (mtSto))))

(test (interp (parse '{{fun {b}
                            {seqn
                             {setbox b {+ 2 (openbox b)}}
                             {setbox b {+ 3 (openbox b)}}
                             {setbox b {+ 4 (openbox b)}}
                             {openbox b}}}
                       {newbox 1}})
              (mtSub)
              (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))


(test/exn (interp (parse '{openbox x})
                  (aSub 'x (boxV 1) (mtSub))
                  (mtSto))
          "unallocated")

;; records

(test (interp-expr (parse '{{fun {r}
                                 {get r x}}
                            {rec {x 1}}}))
      1)

(test (interp-expr (parse '{{fun {r}
                                 {seqn
                                  {set r x 5}
                                  {get r x}}}
                            {rec {x 1}}}))
      5)
(test (interp-expr (parse '{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}}            ; g
                              {fun {r} {fun {v} {set r b v}}}} ; s
                             {rec {a 0} {b 2}}}                ; r1
                            {rec {a 3} {b 4}}}))               ; r2
      5)

(test (interp-expr (parse '{fun {x} x}))
      'func)
(test (interp-expr (parse '{newbox 1}))
      'box)
(test (interp-expr (parse '{rec}))
      'record)