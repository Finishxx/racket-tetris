#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn)
(provide (all-defined-out))

;; Tet -> Bool
;; ends the game, if blocks reach y = 21 or 21 <= y
;; 1. Ends game => y => 21 #t
;; 2. Is ok => #f
(define (end-game? tet)
  (cond
    [(top-off? (tet-blocks tet)) #t]
    [else #f]))

;; ListOf(Posn)(tet-blocks) -> Bool
;; 1. y => 21 #t
;; 2. else #f
(define (top-off? blocks)
  (cond
    [(empty? blocks) #f]
    [(or (posn>=21? (first blocks))
         (top-off? (rest blocks))) #t]
    [else #f]))

;; Posn(posn-y(tet-blocks)) -> Bool
;; returns true, if y is >= 21
(define (posn>=21? pos)
  (>= (posn-y (block-posn pos)) 21))

;; Tet -> Img
;; returns the last state of the world
(define (last-frame tet)
  (place-images
   (list
    GAME-OVER
    (text (string-append "SCORE: " (number->string (score-score (tet-score tet)))) (* 2.5 CUBE-LENGTH) "white")
    (text (string-append "LEVEL: " (number->string (score-level (tet-score tet)))) (* 3 CUBE-LENGTH) "white")
    (text "Game made by Martin Tománek" CUBE-LENGTH "gray"))
   (list
    (make-posn (half (image-width LAST-MTSC)) (half (half (image-height LAST-MTSC))))
    (make-posn (half (image-width LAST-MTSC)) (* (image-height LAST-MTSC) 3/4))
    (make-posn (half (image-width LAST-MTSC)) (half (image-height LAST-MTSC)))
    (make-posn (half (image-width LAST-MTSC)) (* (image-height LAST-MTSC) 9/10)))
   LAST-MTSC))
    