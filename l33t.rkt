#lang racket

;Template: (define (replace-numbers lofw) (cond [(empty? lofw) ...] [else ... (first lofw) ... (replace-numbers (rest lofw)) ...]))

;Function: (define (replace-numbers lofw) (cond [(empty? lofw) ...] [else ... (first lofw) ... (replace-numbers (rest lofw)) ...]))

;(check-expect (replace-numbers '("I" "am" "so" "happy")) '("1" "am" "s0" "h4ppy"))

;(check-expect (replace-numbers '("I" "am" "so" "happy" "today")) '("1" "am" "s0" "h4ppy" "t0d4y"))

;(define (replace-new los)
;  (cond [(empty? los) empty]
;        [(char=? (first los) #\A) (cons #\4 (replace-new (rest los)))]
;        [(char=? (first los) #\a) (cons #\4 (replace-new (rest los)))]
;        [(char=? (first los) #\E) (cons #\3 (replace-new (rest los)))]
;        [(char=? (first los) #\e) (cons #\3 (replace-new (rest los)))]
;        [(char=? (first los) #\I) (cons #\1 (replace-new (rest los)))]
;        [(char=? (first los) #\i) (cons #\1 (replace-new (rest los)))]
;        [(char=? (first los) #\O) (cons #\0 (replace-new (rest los)))]
;        [(char=? (first los) #\o) (cons #\0 (replace-new (rest los)))]
;        [else (cons (first los) (replace-new (rest los)))]
;  ))




;Consumes a list of words (represented as strings) and produces a list of the same words, in the same order, but where some of the letters have been replaced by characters that stand for numbers. Specifically, it turns #\A and #\a into #\4,
;Note that #\4 is a character, whereas 4 is a number. You can’t do arithmetic on the former or put the latter in a string.
; #\E and #\e into #\3, #\I and #\i into #\1, and #\O and #\o into #\0.

;Examples: (replace-numbers '("I" "am" "so" "happy")) => ("1" "am" "s0" "h4ppy")
;          (replace-numbers '("I" "am" "so" "happy" "today")) => ("1" "am" "s0" "h4ppy" "t0d4y")
;          '() => '()

;Data Definitions: A word is a string of letters, digits, and/or underscores, beginning with a letter.

(define (replace-numbers lofw)
  (map (lambda(x) (if (equal? (replaced-list (string->list x)) '())
                      ""
                      (list->string (replaced-list (string->list x))))) lofw))

(check-expect (replace-numbers '("Hello")) '("H3ll0"))


; Char -> Boolean
; tests if c is a vowel 
(define (is-vowel? c)
  (ormap (lambda (x) (member? x '(#\a #\e #\i #\o #\A #\E #\I #\O))) (cons c '())))

(check-expect (is-vowel? #\a) true)
(check-expect (is-vowel? #\b) false)
(check-expect (is-vowel? #\A) true)


; List-of-chars -> List-of-chars
; filters l for vowels and replaces them with numbers

(check-expect (replaced-list (cons #\I '())) (cons #\1 '()))
(check-expect (replaced-list (cons #\a '())) (cons #\4 '()))
(check-expect (replaced-list (cons #\4 '())) (cons #\4 '()))
(check-expect (replaced-list (cons #\C '())) (cons #\C '()))

(define (replaced-list l)
    (map (lambda (x) (cond [(is-vowel? x) (cond     [(char=? x #\a) #\4]
                                                    [(char=? x #\A) #\4]
                                                    [(char=? x #\e) #\3]
                                                    [(char=? x #\E) #\3]
                                                    [(char=? x #\i) #\1]
                                                    [(char=? x #\I) #\1]
                                                    [(char=? x #\o) #\0]
                                                    [(char=? x #\O) #\0]
                                                )]
                                 [else x])) l))