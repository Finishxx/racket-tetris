#lang racket
(require "const+aux.rkt"
         lang/posn)

(provide (all-defined-out))


;;Tetromino start locations

;;    The I and O spawn in the middle columns
;;    The rest spawn in the left-middle columns
;;    The tetriminoes spawn horizontally with J, L and T spawning flat-side first.
;;    Spawn above playfield, row 21 for I, and 21/22 for all other tetriminoes.
;;    Immediately drop one space if no existing Block is in its path

;; I - light blue middle collumn
(define I (list (make-block (make-posn 4 21) "light blue")
                (make-block (make-posn 5 21) "light blue")
                (make-block (make-posn 6 21) "light blue")
                (make-block (make-posn 7 21) "light blue")))

;; J -dark blue !__
(define J (list (make-block (make-posn 5 21) "dark blue")
                (make-block (make-posn 4 22) "dark blue")
                (make-block (make-posn 4 21) "dark blue")
                (make-block (make-posn 6 21) "dark blue")))

;; L - orange
(define L (list (make-block (make-posn 5 21) "orange")
                (make-block (make-posn 6 22) "orange")
                (make-block (make-posn 4 21) "orange")             
                (make-block (make-posn 6 21) "orange")))

;; O - yellow
(define O (list (make-block (make-posn 5 22) "yellow")
                (make-block (make-posn 6 22) "yellow")
                (make-block (make-posn 5 21) "yellow")
                (make-block (make-posn 6 21) "yellow")))
;; S - Green
(define S (list (make-block (make-posn 5 21) "green")
                (make-block (make-posn 5 22) "green")
                (make-block (make-posn 6 22) "green")
                (make-block (make-posn 4 21) "green")))
;; Z - Red
(define Z (list (make-block (make-posn 5 21) "red")
                (make-block (make-posn 4 22) "red")
                (make-block (make-posn 5 22) "red")                            
                (make-block (make-posn 6 21) "red")))
;; T - magenta
(define T (list (make-block (make-posn 5 21) "magenta")
                (make-block (make-posn 5 22) "magenta")
                (make-block (make-posn 4 21) "magenta")                
                (make-block (make-posn 6 21) "magenta")))

(define DEFAULT-BAG (list I J L O S Z T))

