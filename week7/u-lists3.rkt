#lang racket/base

(define (take xs n)
  (void))

(define (drop xs n)
  (void))



(define (chunk n xs)
  (define (safe-take n xs)
    (cond ((> n (length xs)) xs)
          (else (take xs n)))
  )

  (define (safe-drop n xs)
    (cond ((> n (length xs)) '())
          (else (drop xs n))))
  
  (if (null? xs)
      '()
      (cons (safe-take n xs) (chunk n (safe-drop n xs))))
)

(define (fold-right op nv xs)
  (if (null? xs)
      nv
      (op (car xs) (fold-right op nv (cdr xs)))))

(define (fold-left op nv xs)
  (if (null? xs)
      nv
      (fold-left op (op nv (car xs)) (cdr xs))))

(define (reverse l)
  (foldl cons '() l))


(define (filter p? xs)
  (fold-right (lambda (curr res) (cond ((p? curr) (cons curr res))
                                       (else res)) )
              '()
              xs)
)

(define (map* f xs)
  (fold-right (lambda (curr res) (cons (f curr) res))
              '()
              xs))



(require rackunit)
(require rackunit/text-ui)

; flatten
; функция, която премахва допълнителни нива на влагане в списъци
; за да я имплементираме, е хубаво да видим какво правят функциите atom? и/или list?
; хубаво е и да помислим дали не може да използваме някоя от map/filter/fold за проблема

(define (atom? x)
  (and (not null? x) (not (pair? x))))

(define (get-atoms xs)
  (if (atom? xs)
      xs
      (cons (get-atoms (car xs)) (get-atoms (cdr xs)))
      ))

(define (flatten xs)
  (define (helper l)
    (cond ((null? l) '())
          ((list? (car l)) (append (helper (car l)) (helper (cdr l))))
          (else (append (list (car l)) (helper (cdr l))))))
  (helper xs)
)


(define flatten-tests
  (test-suite "flatten"
    (check-equal? (flatten '((1) 2 ((3 4 (5)) (((((6))))))))  '(1 2 3 4 5 6))
    (check-equal? (flatten '(1 2 3 (4)))  '(1 2 3 4))
    (check-equal? (flatten '(() () 3 (2))) '(3 2))
  )
)

;(run-tests flatten-tests 'verbose)


; zip-with
; като zip, но не е задължително да правим двойки, а даваме функция, която да комбинира елементите - тестовете са добър източник на примери

(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys))))
)

;if the lists are the same length
(define (zip-with* f xs ys)
  (map f xs ys))


(define tests-zip
  (test-suite "zip-with"
    (check-equal? (zip-with + '(1 2 3) '(4 5 6)) '(5 7 9))
    (check-equal? (zip-with * '(28 9 12) '(1 3)) '(28 27))
  )
)

;(run-tests tests-zip 'verbose)


; Изпълнител, Име, Популярност, Продължителност(мс), танцувалност, акустичност
(define tracks '(
("Mac Miller" "Self Care" 78 345133 0.52 0.367)
("Mac Miller" "Small Worlds" 67 271733 0.516 0.814)
("Hayley Williams" "Simmer" 60 266115 0.79 0.218)
("Grimes" "My Name is Dark - Art Mix" 55 356077 0.518 0.00288)
("Gorillaz" "Momentary Bliss (feat. slowthai and Slaves)" 61 221386 0.641 0.0851)
("The Neighbourhood" "Wires" 69 193080 0.568 0.0004)
("Interpol" "Rest My Chemistry" 65 301093 0.47 0.0185)
("Massive Attack" "Protection" 59 471560 0.577 0.0162)
("MGMT" "Little Dark Age" 79 299960 0.705 0.0102)
("Arcade Fire" "Ready to Start" 63 255893 0.282 0.12)
("Gorillaz" "November Has Come" 59 165093 0.702 0.0815)
("Foals" "Exits" 60 357419 0.483 0.0559)
("Editors" "Cold" 43 218766 0.595 0.0109)
("Thundercat" "Them Changes" 72 188453 0.657 0.54)
("Anderson .Paak" "Make It Better (feat. Smokey Robinson)" 67 219332 0.676 0.0112)
("Aminé" "Spice Girl" 69 173946 0.693 0.392)
("Mac Miller" "What's the Use?" 71 288640 0.759 0.736)
("Mac Miller" "2009" 69 347986 0.533 0.77)
("Massive Attack" "Angel" 62 379533 0.714 0.0157)
("The Roots" "You Got Me" 66 259306 0.729 0.151)
("SZA" "Drew Barrymore" 69 231400 0.578 0.493)
("Childish Gambino" "Redbone" 82 326933 0.743 0.167)
("Los Bitchos" "The Link Is About to Die" 42 228653 0.606 0.0738)
("Gorillaz" "Tranz" 64 162626 0.649 0.0324)
("Jamiroquai" "Everyday - Remastered" 43 268466 0.703 0.342)
("Amanda Palmer" "Drowning In The Sound" 31 345000 0.593 0.258)
("Massive Attack" "Dissolved Girl" 52 366893 0.646 0.0351)
("Gorillaz" "El Mañana" 63 235360 0.644 0.000988)
("Empress Of" "Maybe This Time" 33 176160 0.69 0.118)
("Run The Jewels" "pulling the pin (feat. Mavis Staples & Josh Homme)" 53 217240 0.468 0.291)
("Arctic Monkeys" "American Sports" 50 158040 0.372 0.00169)
("Fiona Apple" "Cosmonauts" 50 239310 0.547 0.702)
("How To Destroy Angels" "The Space In Between" 44 214509 0.515 0.388)
("Lorde" "The Louvre" 58 271088 0.664 0.239)
("Sevdaliza" "Lamp Lady" 50 211267 0.67 0.17)
("Freddie Gibbs" "1985" 57 152546 0.528 0.337)
("Sevdaliza" "Rhode" 45 202222 0.47 0.000664)
("Soccer Mommy" "Your Dog" 59 194009 0.652 0.122)
("Daft Punk" "Instant Crush (feat. Julian Casablancas)" 75 337560 0.775 0.0422)
("The Carters" "SUMMER" 49 285200 0.579 0.114)
("Aesop Rock" "None Shall Pass" 56 243626 0.707 0.184)
("PVRIS" "Use Me (feat. 070 Shake)" 60 203026 0.315 0.0353)
("Joji" "Run" 75 195000 0.448 0.0427)
))

; Задачки

; Да вземем всички песни с времетраене, повече от 240 секунди

(define (longer-tracks tracks)
  (filter (lambda (x) (> (cadddr x) 240000)) tracks)
)


; Да вземем само имената на горните песни?

(define (names-of-longer-tracks tracks)
  (let (
        (info (longer-tracks tracks))
        )
    (foldl (lambda (x res) (append res (list (car x)))) '() info)
    )
)

; Да направим времетраенето на всяка песен в секунди
(define (duration-in-seconds tracks)
  (map (lambda (x) (append (list (cadr x)) (list (quotient (cadddr x) 1000)))) tracks)
)

; Да намерим най-популярната песен

(define (most-popular-track tracks)
  (foldr (lambda (x res) (if (> (caddr x) (cadr res)) (cons (cadr x) (cons (caddr x) '())) res)) (list "" 0) tracks)
)

; Да намерим средната популярност на песните

(define (average-popularity tracks)
  (quotient (foldr (lambda (x res) (+ (caddr x) res)) 0 tracks) (length tracks))
)

; Всички песни с популярност под средната

(define (hipster-tracks tracks)
  (void)
)

; Всички песни с популярност над средната

(define (mainstream-tracks tracks)
  (void)
)

; А да не повтаряме код между горните?

; Да комбинираме името на песента и изпълнителя в един низ - <изпълнител> - <име>

(define (combined-artist-and-track-name tracks)
  (void)
)