;Consumes a list of numbers and produces the same list, but where any “run” of numbers (i.e., consecutive numbers that are identical) is replaced by a single occurrence of that number
;singles :: List-of-numbers -> List-of-numbers

;Examples: (list 1, 9, 3, 10, 4, 20, 2) -> (list 1, 9, 3, 10, 4, 20, 2)
;          (list 1, 1, 1, 1, 1, 1, 1) -> (list 1)
;          (list 1, 1, 1, 2, 2, 2, 1) -> (list 1, 2, 1)
;          '() -> '()

;Template: (define (fn lox) ... (cond [(empty? lox) ...] [(cons? lox) ... (first lox) ... (fn (rest lox)) ...]))

(define (singles lox)
  (cond [(empty? lox) empty]
        [(cons? lox) (cond [(empty? (rest lox)) (list (first lox))]
                           [(= (first lox) (first (rest lox))) (singles (rest lox))]
                           [else (cons (first lox) (singles (rest lox)))] )] ) )