#lang plai

;1)
;dollar->won: number -> number
;to convert the given number of dollars to wons
;with the conversion rate won/dollar=1100
(define (dollar->won usd)
  (* usd 1100))
(test (dollar->won 1) 1100)
(test (dollar->won 10) 11000)

;2)
;volume-cuboid: number number number -> number
;to compute the volume of a cube with the length of three sides given
(define (volume-cuboid a b c)
  (* a b c))
(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 10 15 8) 1200)

;3)
;is-even? number -> boolean
;to determine if the given number is even
(define (is-even? a)
  (= (modulo a 2) 0))
(test (is-even? 5) #f)
(test (is-even? 1120) #t)

;4)
;gcd: number number -> number
;to find the greatest common divisor of 2 numbers
(define (gcd a b)
  ;Euclid's algorithm https://en.wikipedia.org/wiki/Greatest_common_divisor#Using_Euclid's_algorithm
  (cond
    [(= b 0) a]
    [else (gcd b (modulo a b))]
    )
  )
(test (gcd 5 0) 5)
(test (gcd 0 10) 10)
(test (gcd 54 24) 6)

;5)
;lcm: number number -> number
;to find the least common multiple of 2 given numbers
(define (lcm a b)
  ;https://en.wikipedia.org/wiki/Least_common_multiple#Reduction_by_the_greatest_common_divisor
  (cond
    [(and (= a 0) (= b 0)) 0]
    [else (/ (abs (* a b)) (gcd a b))]
    )
  )
(test (lcm 21 6) 42)
(test (lcm 0 0) 0)
(test (lcm 5 0) 0)

;6)
;COURSE->number or COURSE->number number
;to define the new type of COURSE which is either CS320, CS311 or CS330 with the
;given arguments
(define-type COURSE
  [CS320 (quiz number?)
         (homework number?)]
  [CS311 (homework number?)]
  [CS330 (projects number?)
         (homework number?)])
;7)
;have-homework: COURSE -> number
;to retrieve the number of homeworks for the given course
(define (have-homework class)
  (type-case COURSE class
    [CS320 (q hw) hw]
    [CS311 (hw) hw]
    [CS330 (pr hw) hw]))
(test (have-homework (CS311 2)) 2)
(test (have-homework (CS330 10 5)) 5)
(test (have-homework (CS320 6 7)) 7)

;8)
;have-projects: COURSE -> boolean
;to know if the given course is CS330
;and has 2 or more projects (true in this case, false otherwise)

(define (have-projects class)
  (type-case COURSE class
    [CS320 (q hw) #f]
    [CS311 (hw) #f]
    [CS330 (pr hw)
           (cond
             [(>= pr 2) #t]
             [else #f])]))

(test (have-projects (CS311 2)) #f)
(test (have-projects (CS330 10 5)) #t)
(test (have-projects (CS330 1 5)) #f)
(test (have-projects (CS320 6 7)) #f)

;9)
;name-pets: list -> list
;to change in the list of pets 'dog to 'happy, 'cat to 'smart, 'pig to 'pinky
;and leave all the other pets unnamed
(define (name-pets pets)
  (cond
    [(empty? pets) empty]
    [(list? pets) (cons
                   (cond
                     [(symbol=? (first pets) 'dog) 'happy]
                     [(symbol=? (first pets) 'cat) 'smart]
                     [(symbol=? (first pets) 'pig) 'pinky]
                     [else (first pets)])
                   (name-pets (rest pets)))]))

(test (name-pets '(cat dog pig)) '(smart happy pinky))
(test (name-pets '(a b c cat)) '(a b c smart))
(test (name-pets '()) '())

;10)
;name-pets: symbol symbol list->list
;to name the given pet with the given name in the list of pets
(define (give-name pet name pets)
  (cond
    [(empty? pets) empty]
    [(list? pets) (cons
                   (cond
                     [(symbol=? (first pets) pet) name]
                     [else (first pets)])
                   (give-name pet name (rest pets)))]))
(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty)))) '(pig cat pooh))
(test (give-name 'pokemon 'pikachu '(cat pokemon bear pokemon)) '(cat pikachu bear pikachu))
(test (give-name 'a 'b '()) '())

  
