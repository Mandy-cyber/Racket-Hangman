;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MISC FUNCTIONS
;;;-----------------

; choose-random : (X) [ListOf X] -> X
; Chooses a random thing from a list
(define (choose-random lox)
  (local [(define len-lox (length lox))
          (define n (random len-lox))]

    (list-ref lox n)))

; possible-words: String -> [ListOf String]
; Gets all the possible words from a given file path
(define (possible-words fp)
  (if (file-exists? fp)
      (filter (λ (l) (<= (string-length l) 6)) (read-lines fp))
      '()))

#;(define POSSIBLE-WORDS (possible-words "words.txt"))

; get-score: String -> Nat
; Gets a user's current score from a given file path
(define (get-score fp)
  (if (file-exists? fp)
      (local [(define lines (read-lines fp))]
        (if (empty? lines) 0
            (string->number (first lines))))
      0))


; change-score: String Nat [[ListOf Number] -> Number]-> String
; Updates the score seen in a given file.
(define (change-score fp n op)
  (local [(define new-score (op (get-score fp) n))]
    (write-file fp (number->string new-score))))


; same-list? : [ListOf String] [ListOf String] -> Boolean
; Do two lists contain the same letters?
(define (same-list? los1 los2)
  (and (= (length los1) (length los2))
       (andmap (λ (s) (string-in-list? s los2)) los1)))

(check-expect (same-list? '() '()) #t)
(check-expect (same-list? (list "a") (list "a")) #t)
(check-expect (same-list? (list "a" "b" "c") (list "b" "a" "c")) #t)
(check-expect (same-list? (list "a" "b" "c") (list "b")) #f)
(check-expect (same-list? (list "a" "b" "c") (list "d" "a" "c")) #f)


; string-in-list? : String [ListOf String] -> Boolean
; Is a given string in a list?
(define (string-in-list? s los)
  (ormap (λ (x) (string=? s x)) los))

(check-expect (string-in-list? "" '()) #f)
(check-expect (string-in-list? "s" '()) #f)
(check-expect (string-in-list? "a" (list "a" "g" "a")) #t)
(check-expect (string-in-list? "x" (list "a" "x" "a")) #t)
(check-expect (string-in-list? "f" (list "a" "x" "a")) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notes:
; - Hangman has a correct word, the user's guess(es), and a
;   drawing of the hangman depending on how many guesses they
;   have gotten wrong.
; - A guess can either be a word or a letter. They both count
;   equally (i.e same loss on the diagram).
; - Each wrong guess increments a wrong guess counter. Each
;   value in the counter corresponds to a part of the hangman
;   diagram.
; - Game should randomly generate the correct word based on a
;   search of the POSSIBLE-WORDS, which I can later expand to
;   the full dictionary (if i so please?).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS & INTERPRETATIONS
;;;-------------------------------------

; A Guess is one of:
; - 1String
; - String
; Interpretation: A user's guess can either be a single letter or
;                 the whole word. No empty strings.


(define-struct hangman [correct current guesses diagram word score hints])
; A game of Hangman is a (make-hangman String String [ListOf Guess] 
;                                      Image Image Nat [ListOf 1String])
; correct is the correct word to be guessed
; current is what the user is currently typing
; guesses is the list of guesses the user has made
; diagram is the image of the hangman
; word is an image of what the user has guessed so far
; score is how many points the user has earned from playing hangman so far
; hints is the list of hints the user has gotten (letters in the word)
; Interpretation: A game of hangman, with a maximum of 6 user guesses allowed

;;; CONSTANTS
;;;-----------
(define BG (rectangle 900 800 "solid" "beige"))
(define BG-LOST (rectangle 900 800 "solid" "pink"))
(define BG-WON (rectangle 900 800 "solid" "green"))
(define LETTER-SQUARE (square 75 "solid" "pink"))
(define GAP (rectangle 10 75 "solid" "beige"))
(define H-GAP (rectangle 900 50 "solid" "beige"))
(define G-GAP (rectangle 900 50 "solid" "green"))
(define L-GAP (rectangle 900 50 "solid" "pink"))
(define GUESSES-ALLOWED 6)
(define FONT-SIZE 24)
(define FONT-SIZE15 15)
(define FONT-COLOR "white")
(define FONT-COLOR2 "black")
(define LETTER "letter")
(define WORD "word")
(define note (text "For 20 points you can get a hint. Press the UP-key."
                   15 "black"))

; single parts of the hangman
(define HEAD (overlay (circle 40 "solid" "white")
                      (circle 45 "solid" "black")))
(define BODY (rectangle 5 150 "solid" "black"))
(define RIGHT-ARM (rotate
                   45 (rectangle 5 75 "solid" "black")))
(define LEFT-ARM (rotate
                  315 (rectangle 5 75 "solid" "black")))
(define RIGHT-LEG (rotate
                   45 (rectangle 5 75 "solid" "black")))
(define LEFT-LEG (rotate
                  315 (rectangle 5 75 "solid" "black")))

; multiple parts of the hangman
(define HEAD&BODY (above HEAD BODY))

(define HEAD&BODY&LLEG
  (overlay/offset LEFT-LEG 25 -145 HEAD&BODY))

(define HEAD&BODY&2LEGS
  (overlay/offset RIGHT-LEG -29 -118 HEAD&BODY&LLEG))

(define HEAD&BODY&LEGS&LARM
  (overlay/offset LEFT-ARM 25 -10 HEAD&BODY&2LEGS))

(define FULL-HANGMAN
  (overlay/offset RIGHT-ARM -28 -12 HEAD&BODY&LEGS&LARM))



;;; EXAMPLES
;;;-----------
#;(define POSSIBLE-WORDS
    (list "balloon" "zen" "run" "righteousness" "above"))

(define GUESS1 "a")
(define GUESS2 "balloon")
(define GUESS3 "run")

; game examples come later once some of the functions have been made

;;; TEMPLATES
;;;-----------
#;(define (guess-temp g)
    (if (> (string-length g) 1)
        ....
        ....))

#;(define (guesses-temp guesses)
    (cond
      [(empty? guesses)...]
      [(cons? guesses)
       (...(guess-temp (first guesses))...
           (guesses-temp (rest guesses)))]))

(define (hints-temp hints)
  (cond
    [(empty? hints)...]
    [(cons? hints)
     (...(hints-temp (first hints))...
         (hints-temp (rest hints)))]))

#;(define (game-temp g)
    (...(hangman-correct g)...
        (hangman-current g)...
        (guesses-temp (hangman-guesses g))...
        (hangman-diagram g)...
        (hangman-word g)...
        (hangman-score g)...
        (hints-temp (hangman-hints g))...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HELPER FUNCTIONS
;----------------------------------------------------------------

; letter/word? : Guess -> String
; Determines whether a guess is a word or a single letter
(define (letter/word? g)
  (local [(define len (string-length g))]
    (cond
      [(= len 0) (error "A guess cannot be an empty string")]
      [(= len 1) LETTER]
      [else WORD])))


(check-expect (letter/word? "a") LETTER)
(check-expect (letter/word? "A") LETTER)
(check-expect (letter/word? "as") WORD)
(check-expect (letter/word? "rHOmBuS") WORD)
(check-error (letter/word? "")
             "A guess cannot be an empty string")

;----------------------------------------------------------------

; in-word? : 1String String -> Boolean
; Checks if a letter is in a word
(define (in-word? letter word)
  (ormap (λ (w) (string-ci=? w letter)) (explode word)))

(check-expect (in-word? "a" "") #f)
(check-expect (in-word? "r" "run") #t)
(check-expect (in-word? "x" "zebra") #f)
(check-expect (in-word? "x" "xylophone") #t)

;----------------------------------------------------------------

; guess-in-word? : Guess String -> Boolean
; Checks if a given guess is in the correct word
(define (guess-in-word? guess s)
  (local [(define g-type (letter/word? guess))]

    (if (string=? g-type LETTER)
        (ormap (λ (l) (in-word? l s)) (explode guess))
        (string-ci=? guess s))))

(check-expect (guess-in-word? "a" "balloon") #t)
(check-expect (guess-in-word? "A" "balloon") #t)
(check-expect (guess-in-word? "goat" "goat") #t)
(check-expect (guess-in-word? "z" "run") #f)
(check-expect (guess-in-word? "x" "zebra") #f)
(check-expect (guess-in-word? "boat" "goat") #f)

;----------------------------------------------------------------

; word-boxes : Nat -> Image
; Makes boxes for a user's guess depending on how many letters in the word
(define (word-boxes n)
  (local [(define box-list (make-list n LETTER-SQUARE))]
    (if (zero? n) empty-image
        (foldl (λ (i accum) (beside i GAP accum))
               GAP
               box-list))))

(check-expect (word-boxes 0) empty-image)
(check-expect (word-boxes 1) (beside LETTER-SQUARE GAP GAP))
(check-expect (word-boxes 2) (beside LETTER-SQUARE GAP
                                     LETTER-SQUARE GAP GAP))
(check-expect (word-boxes 3) (beside LETTER-SQUARE GAP
                                     LETTER-SQUARE GAP
                                     LETTER-SQUARE GAP GAP))

;----------------------------------------------------------------

; fill-in-letter: 1String String -> [ListOf 1String]
; Place a letter in their correct space based on the
; correct word. 
(define (fill-in-letter letter word)
  (map
   (λ (l) (if (string=? letter l) l ""))
   (explode word)))

(check-expect (fill-in-letter "a" "donkey")
              (starting-accum (string-length "donkey")))
(check-expect (fill-in-letter "a" "abs")
              (list "a" "" ""))
(check-expect (fill-in-letter "s" "songs")
              (list "s" "" "" "" "s"))

;----------------------------------------------------------------

; starting-accum: Nat -> [ListOf String]
; Creates a list of size n of empty strings.
(define (starting-accum n) (make-list n ""))

(check-expect (starting-accum 0) '())
(check-expect (starting-accum 1) (list ""))
(check-expect (starting-accum 5) (list "" "" "" "" ""))
  
;----------------------------------------------------------------
  
; replace-if-empty : [ListOf [ListOf 1String]] [ListOf String] -> [ListOf 1String]
; Replace empty strings with the correct letter if applicable
(define (replace-if-empty lo1s accum)
  (local [(define good-guesses
            (filter (λ (los)
                      ; check that at least one 1string in the list is not nth
                      (ormap (λ (x) (not (string=? x ""))) los))
                    lo1s))]

    (cond
      [(empty? good-guesses) accum]
      [(cons? good-guesses)
       (if (empty? lo1s) accum
           (local [(define (return-what a b)
                     (cond
                       ; both 1Strings are empty
                       [(and (string=? a "")
                             (string=? b "")) ""]
      
                       ; first 1String is empty, second not empty
                       [(and (string=? a "")
                             (not (string=? b ""))) b]
      
                       ; first 1String is not empty, second is empty
                       [(and (not (string=? a ""))
                             (string=? b "")) a]

                       ; the strings are equal
                       ; e.g if you tried a letter guess then a word guess
                       [(string=? a b) a]))]

             (replace-if-empty
              (rest good-guesses)
              (map return-what (first good-guesses) accum))))])))


; fill-in-letters: [ListOf Guess] String -> [ListOf 1String]
; Place non-repeated letters in their correct spaces
; based on the correct word
(define (fill-in-letters guesses word)
  (local [(define (add-if-no-duplicate gs)
            (foldr (λ (x accum)
                     (cond
                       [(cons? x)
                        (if (empty? x) accum (append x accum))]
                       [else (append (list x) accum)]))
                   '() gs))
          
          (define letters
            (add-if-no-duplicate
             (map (λ (g)
                    (if (string=? (letter/word? g) LETTER)
                        g (explode g)))
                  guesses)))]
 
    (replace-if-empty
     (map (λ (letter) (fill-in-letter letter word)) letters)
     (starting-accum (string-length word)))))


(check-expect (fill-in-letters (list "a" "r" "z") "zebra")
              (list "z" "" "" "r" "a"))

(check-expect (fill-in-letters (list "b") "girl")
              (list "" "" "" ""))

(check-expect (fill-in-letters (list "b") "nonbinary")
              (list "" "" "" "b" "" "" "" "" ""))

(check-expect (fill-in-letters (list "b" "k" "s" "o") "books")
              (list "b" "o" "o" "k" "s"))

(check-expect (fill-in-letters (list "c" "o" "t" "cat") "cat")
              (list "c" "a" "t"))

(check-expect (fill-in-letters (list "dog" "cat") "ragged")
              (list "" "a" "g" "g" "" "d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DRAWING FUNCTIONS
;;;------------------

; letters->image : [ListOf String] String -> Image
; Visualize the correct positions of given letters in a correct word
(define (letters->image lo1s correct)
  (if (empty? lo1s) (word-boxes (string-length correct))
      
      (local [(define letters (fill-in-letters lo1s correct))
              (define letter-boxes
                (map (λ (letter) (overlay (text letter FONT-SIZE FONT-COLOR)
                                          LETTER-SQUARE)) letters))]

        (foldr (λ (x accum) (beside x GAP accum)) GAP letter-boxes))))


(check-expect (letters->image '() "hi")
              (beside LETTER-SQUARE GAP
                      LETTER-SQUARE GAP GAP))

(check-expect (letters->image (list "a" "r" "z") "zebra")
              (beside
               (overlay (text "z" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "r" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "a" 24 "white") LETTER-SQUARE)
               GAP GAP))

(check-expect (letters->image (list "b" "k" "s" "o") "books")
              (beside
               (overlay (text "b" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "o" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "o" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "k" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "s" 24 "white") LETTER-SQUARE)
               GAP GAP))

(check-expect (letters->image (list "y") "abs")
              (beside
               (overlay (text "" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "" 24 "white") LETTER-SQUARE)
               GAP GAP))

(check-expect (letters->image (list "yes") "yes")
              (beside
               (overlay (text "y" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "e" 24 "white") LETTER-SQUARE)
               GAP
               (overlay (text "s" 24 "white") LETTER-SQUARE)
               GAP GAP))

;----------------------------------------------------------------

; guesses->hangman: Nat -> Image
; Visualize the hangman diagram based on how many wrong-guesses
; have been made.
(define (guesses->hangman n)
  (cond
    [(zero? n) empty-image]
    [(positive? n)
     (cond
       [(= n 6) HEAD]
       [(= n 5) HEAD&BODY]
       [(= n 4) HEAD&BODY&LLEG]
       [(= n 3) HEAD&BODY&2LEGS]
       [(= n 2) HEAD&BODY&LEGS&LARM]
       [(= n 1) FULL-HANGMAN])]))

(check-expect (guesses->hangman 0) empty-image)
(check-expect (guesses->hangman 6) HEAD)
(check-expect (guesses->hangman 5) HEAD&BODY)
(check-expect (guesses->hangman 4) HEAD&BODY&LLEG)
(check-expect (guesses->hangman 3) HEAD&BODY&2LEGS)
(check-expect (guesses->hangman 2) HEAD&BODY&LEGS&LARM)
(check-expect (guesses->hangman 1) FULL-HANGMAN)

;----------------------------------------------------------------

; hints->text : [ListOf String] -> Image
; Visualizes text of the hints a user has.
(define (hints->text los)
  (local [(define (make-text t) (text t FONT-SIZE15 FONT-COLOR2))]
    (if (empty? los) (make-text "Your hints: ")
        (local [(define hints-text
                  (foldl
                   (λ (h accum) (string-append accum h ", "))
                   "Your hints: "
                   (remove "" los)))]

          (make-text hints-text)))))

(check-expect (hints->text '())
              (text "Your hints: " FONT-SIZE15 FONT-COLOR2))

(check-expect (hints->text (list ""))
              (text "Your hints: " FONT-SIZE15 FONT-COLOR2))

(check-expect (hints->text (list "g"))
              (text "Your hints: g, " FONT-SIZE15 FONT-COLOR2))

(check-expect (hints->text (list "g" "a" "c"))
              (text "Your hints: g, a, c, " FONT-SIZE15 FONT-COLOR2))

;----------------------------------------------------------------

(define GAME0
  (make-hangman
   "game"
   ""
   '()
   empty-image
   (letters->image '() "game")
   50
   '()))


(define GAME1
  (make-hangman
   "balloon"
   "x"
   (list "r" "a" "l" "z")
   HEAD&BODY
   (letters->image (list "r" "a" "l" "z") "balloon")
   0
   '()))


(define GAME2
  (make-hangman
   "cat"
   "yes"
   (list "sat" "c")
   HEAD
   (letters->image (list "sat" "c") "cat")
   100
   (list "c")))


(define GAME3
  (make-hangman
   "seasonal"
   ""
   (list "b" "g" "r" "j" "i" "c")
   FULL-HANGMAN
   (letters->image (list "b" "g" "r" "j" "i" "c") "seasonal")
   1000
   '()))

;----------------------------------------------------------------

;;; TO-DRAW FUNCTION
;;;-------------------

; game->image : Hangman -> Image
; Visualizes the state of the hangman game
(define (game->image h)
  (local [(define guesses (hangman-guesses h))
          (define num-left (number->string (- 6 (length guesses))))
          (define hints-image (hints->text (hangman-hints h)))
          (define score (number->string (hangman-score h)))
          (define (make-text t) (text t FONT-SIZE FONT-COLOR2))]
    
    (overlay
     (above
      note
      H-GAP
      hints-image
      H-GAP
      (make-text (string-append "Your score is: " score))
      (make-text
       (string-append "You have " num-left " guesses remaining"))
      H-GAP
      (make-text
       (string-append "Current guess: " (hangman-current h)))
      H-GAP
      (hangman-diagram h)
      H-GAP
      (letters->image guesses (hangman-correct h)))
     BG)))


(check-expect (game->image GAME0)
              (overlay
               (above
                note
                H-GAP
                (hints->text (hangman-hints GAME0))
                H-GAP
                (text "Your score is: 50" FONT-SIZE FONT-COLOR2)
                (text "You have 6 guesses remaining" FONT-SIZE FONT-COLOR2)
                H-GAP
                (text (string-append "Current guess: " (hangman-current GAME0))
                      FONT-SIZE FONT-COLOR2)
                H-GAP
                (hangman-diagram GAME0)
                H-GAP
                (letters->image (hangman-guesses GAME0)
                                (hangman-correct GAME0)))
               BG))

(check-expect (game->image GAME1)
              (overlay
               (above
                note
                H-GAP
                (hints->text (hangman-hints GAME1))
                H-GAP
                (text "Your score is: 0" FONT-SIZE FONT-COLOR2)
                (text "You have 2 guesses remaining" FONT-SIZE FONT-COLOR2)
                H-GAP
                (text (string-append "Current guess: " (hangman-current GAME1))
                      FONT-SIZE FONT-COLOR2)
                H-GAP
                (hangman-diagram GAME1)
                H-GAP
                (letters->image (hangman-guesses GAME1)
                                (hangman-correct GAME1)))
               BG))

(check-expect (game->image GAME2)
              (overlay
               (above
                note
                H-GAP
                (hints->text (hangman-hints GAME2))
                H-GAP
                (text "Your score is: 100" FONT-SIZE FONT-COLOR2)
                (text "You have 4 guesses remaining" FONT-SIZE FONT-COLOR2)
                H-GAP
                (text (string-append "Current guess: " (hangman-current GAME2))
                      FONT-SIZE FONT-COLOR2)
                H-GAP
                (hangman-diagram GAME2)
                H-GAP
                (letters->image (hangman-guesses GAME2)
                                (hangman-correct GAME2)))
               BG))

(check-expect (game->image GAME3)
              (overlay
               (above
                note
                H-GAP
                (hints->text (hangman-hints GAME3))
                H-GAP
                (text "Your score is: 1000" FONT-SIZE FONT-COLOR2)
                (text "You have 0 guesses remaining" FONT-SIZE FONT-COLOR2)
                H-GAP
                (text (string-append "Current guess: " (hangman-current GAME3))
                      FONT-SIZE FONT-COLOR2)
                H-GAP
                (hangman-diagram GAME3)
                H-GAP
                (letters->image (hangman-guesses GAME3)
                                (hangman-correct GAME3)))
               BG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; KEY HANDLER FUNCTIONS
;;;------------------------

; backspace : String -> String
; Removes a character from a string
(define (backspace s)
  (if (string=? s "") ""
      (substring s 0 (sub1 (string-length s)))))

(check-expect (backspace "") "")
(check-expect (backspace "a") "")
(check-expect (backspace "cat") "ca")

;----------------------------------------------------------------

; return : String [ListOf Guess] -> [ListOf Guess]
; Adds a guess to the list of guesses if it doesn't already exist
(define (return guess guesses)
  (if (or (ormap (λ (g) (string=? g guess)) guesses)
          (string=? guess ""))
      guesses
      (append (list guess) guesses)))

(check-expect (return "" (list)) (list))
(check-expect (return "" (list "a" "b")) (list "a" "b"))
(check-expect (return "cat" (list)) (list "cat"))
(check-expect (return "cat" (list "cat" "sat" "x")) (list "cat" "sat" "x"))
(check-expect (return "g" (list "a" "b")) (list "g" "a" "b"))

;----------------------------------------------------------------

; other-key : KeyEvent String -> String
; A string of what the user is typing, if valid.
(define (other-key ke word)
  (string-append word
                 (if (or (string-alphabetic? ke)
                         (string=? "-" ke))
                     (string-downcase ke)
                     "")))

(check-expect (other-key "" "") "")
(check-expect (other-key "1" "cat") "cat")
(check-expect (other-key "!" "a") "a")
(check-expect (other-key "-" "high") "high-")
(check-expect (other-key "def" "abc") "abcdef")
(check-expect (other-key "x" "") "x")
(check-expect (other-key "X" "") "x")

;----------------------------------------------------------------

; enough-points? : Nat -> Boolean
; Does a user have a high enough score to use a hint?
; (need 20 points for one hint)
(define (enough-points? n)
  (>= n 20))

(check-expect (enough-points? 0) #f)
(check-expect (enough-points? 19) #f)
(check-expect (enough-points? 20) #t)
(check-expect (enough-points? 21) #t)
(check-expect (enough-points? 57) #t)

;----------------------------------------------------------------

; generate-hint: Hangman -> Hangman
; Generates a hint, which is a letter in the correct word that
; the user has not yet guessed. If there is such a letter, add it
; to the existing list of hints, and update hangman
(define (generate-hint h)
  (local [(define score (hangman-score h))]
    (if (not (enough-points? score))
        h
        (local [(define guesses (hangman-guesses h))
                (define correct (hangman-correct h))
                (define hints (hangman-hints h))
                ; make sure the hint hasnt been guessed and is not an
                ;   existing hint
                (define unguessed-letters
                  (filter
                   (λ (c) (and (not (string-in-list? c guesses))
                               (not (string-in-list? c hints))))
                   (explode correct)))]

          (if (empty? unguessed-letters) h
              (make-hangman
               correct
               (hangman-current h)
               guesses
               (hangman-diagram h)
               (hangman-word h)
               (- score 20)
               (append
                (list (choose-random unguessed-letters))
                hints)))))))



;----------------------------------------------------------------

; key-handler : Hangman KeyEvent -> Hangman
; Adds a user's guess to the game
(define (key-handler h ke)
  (local [(define correct (hangman-correct h))
          (define currently-typing (hangman-current h))
          (define guesses (hangman-guesses h))
          (define diagram (hangman-diagram h))
          (define image (hangman-word h))
          (define score (hangman-score h))
          (define hints (hangman-hints h))
          (define (num-wrong-guesses guesses)
            (- 6 (length (filter
                          (λ (g) (not (guess-in-word? g correct)))
                          guesses))))]

    (cond
      ; backspace
      [(string=? "\b" ke)
       (make-hangman
        correct
        (backspace currently-typing)
        guesses diagram image score hints)]

      ; return
      [(string=? "\r" ke)
       (local [(define new-guesses (return currently-typing guesses) )]
         (if (string=? currently-typing "") h
             (make-hangman
              correct
              ""
              new-guesses
              (guesses->hangman (num-wrong-guesses new-guesses))
              image score hints)))]

      ; up-key (aka get a hint)
      [(string=? "up" ke) (generate-hint h)]

      ; other character
      [else
       (make-hangman
        correct
        (other-key ke currently-typing)
        guesses diagram image score hints)])))


(check-expect (key-handler GAME0 "") GAME0)

(check-expect (key-handler GAME0 "a")
              (make-hangman (hangman-correct GAME0)
                            "a"
                            (hangman-guesses GAME0)
                            (hangman-diagram GAME0)
                            (hangman-word GAME0)
                            (hangman-score GAME0)
                            (hangman-hints GAME0)))

(check-expect (key-handler GAME2 "\b")
              (make-hangman (hangman-correct GAME2)
                            "ye"
                            (hangman-guesses GAME2)
                            (hangman-diagram GAME2)
                            (hangman-word GAME2)
                            (hangman-score GAME2)
                            (hangman-hints GAME2)))

(check-expect (key-handler GAME3 "\r") GAME3)

(check-expect (key-handler GAME1 "\r")
              (make-hangman (hangman-correct GAME1)
                            ""
                            (list "x" "r" "a" "l" "z")
                            (guesses->hangman 3)
                            (hangman-word GAME1)
                            (hangman-score GAME1)
                            (hangman-hints GAME1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STOP-WHEN FUNCTIONS
;;;---------------------

; list=subset? : [ListOf String] [ListOf String] -> Boolean
; Is a list of strings a subset of another list of strings?
(define (list=subset? los1 los2)
  (if (empty? los1) (empty? los2)
      (andmap (λ (s) (string-in-list? s los2)) los1)))

(check-expect (list=subset? '() '()) #t)
(check-expect (list=subset? (list "a") (list "a")) #t)
(check-expect (list=subset? (list "a") (list "b" "a" "c")) #t)
(check-expect (list=subset? (list "a" "c") (list "b" "a" "c")) #t)
(check-expect (list=subset? (list "g") (list "b" "a" "c")) #f)
(check-expect (list=subset? (list "g" "b") (list "b" "a" "c")) #f)


; won-game? : Hangman -> Boolean
; Has the user won the game?
(define (won-game? h)
  (local [(define guesses (hangman-guesses h))
          (define correct (hangman-correct h))
          (define exploded (explode correct))]
    
    (or (string-in-list? correct guesses)
        (list=subset? exploded guesses))))


(check-expect (won-game?
               (make-hangman "cat" "d"
                             (list "c" "d" "t" "a")
                             HEAD
                             (letters->image (list "c" "d" "t" "a") "cat")
                             0 '()))
              #t)

(check-expect (won-game?
               (make-hangman "cat" ""
                             (list "c" "d" "t" "a" "v" "b")
                             HEAD&BODY&LLEG
                             (letters->image (list "c" "d" "t" "a" "v" "b") "cat")
                             50 '()))
              #t)

(check-expect (won-game?
               (make-hangman "dog" ""
                             (list "dog" "t" "a" "v" "b")
                             HEAD&BODY&2LEGS
                             (letters->image (list "dog" "t" "a" "v" "b") "dog")
                             700 '()))
              #t)

(check-expect (won-game?
               (make-hangman "cat" "d"
                             (list "c" "d" "t" "b" "v" "l")
                             HEAD&BODY&2LEGS
                             (letters->image (list "c" "d" "t" "b" "v" "l") "cat")
                             0 '()))
              #f)

(check-expect (won-game?
               (make-hangman "cat" ""
                             '()
                             empty-image
                             (letters->image '() "cat")
                             100 '()))
              #f)


;----------------------------------------------------------------

; end-game? : Hangman -> Boolean
; Should the game end?
(define (end-game? h)
  (local [(define guesses (hangman-guesses h))
          (define correct (hangman-correct h))
          (define won? (won-game? h))]
    (or won?
        (and (not won?)
             (= (length guesses) 6)))))

(check-expect (end-game? GAME0) #f)
(check-expect (end-game? GAME1) #f)
(check-expect (end-game? GAME2) #f)
(check-expect (end-game? GAME3) #t)

;----------------------------------------------------------------

; end-game-drawing : Hangman -> Image
; Show the user if they won or lost
(define (end-game-drawing h)
  (local [(define guesses (hangman-guesses h))
          (define correct (hangman-correct h))
          (define score (hangman-score h))
          (define won? (won-game? h))
          (define (make-text t) (text t FONT-SIZE FONT-COLOR2))]

    (cond
      ; if they won the game
      [won?
       (overlay
        (above (make-text (string-append "You got the correct word, " correct "!!"))
               G-GAP
               (make-text "Points Earned: 50")
               G-GAP
               (make-text (string-append "Total Score: "
                                         (number->string (+ 50 score)))))
        BG-WON)]

      ; if they lost the game
      [(and (= (length guesses) 6) (not won?))
       (overlay
        (above (make-text (string-append "Oops! You ran out of guesses! The correct word was " correct))
               L-GAP
               (make-text "Points Earned: 0")
               L-GAP
               (make-text (string-append "Total Score: " (number->string score))))
        BG-LOST)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BIG-BANG
;;;-----------

; my-game: String String -> String
; Plays a game of hangman with words from one file path and
; score from a different filepath. Updates the score once the
; game is completed.
(define (my-game word-file score-file)
  (local [(define RANDOM-WORD (choose-random (possible-words word-file)))
          (define INIT-GAME
            (make-hangman RANDOM-WORD
                          "" '()
                          empty-image
                          (letters->image '() RANDOM-WORD)
                          (get-score score-file)
                          '()))

          (define game
            (big-bang INIT-GAME
              [to-draw game->image]
              [on-key key-handler]
              [stop-when end-game? end-game-drawing]))

          (define ending-score (hangman-score game))]

    (if (won-game? game)
        (write-file score-file (number->string (+ 50 ending-score)))
        (write-file score-file
                    (number->string ending-score)))))


