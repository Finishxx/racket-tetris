#lang racket
(require 2htdp/universe
         "const+aux.rkt"
         "draw.rkt"
         "tock.rkt"
         "control.rkt"
         "end-game.rkt"
         "tetriminos.rkt")


;; Tetris Guideline : https://tetris.wiki/Tetris_Guideline
;; most Tetris games use grid 10 wide 22 tall where rows above 20 are hidden
;; it is preferable if a sliver of 21st row is visible
;; (https://tetris.wiki/Super_Rotation_System)

(define (tet-main tet)
  (big-bang tet
    [on-key control]
    [on-tick tock (tick-control (score-level (tet-score tet)))]
    [to-draw draw]
    [stop-when end-game? last-frame]))

(define SHUFFLED-BAG (shuffle DEFAULT-BAG))

(define NORMAL-START
  (tet-main (make-tet
             (first SHUFFLED-BAG)
             (list)
             (rest SHUFFLED-BAG)
             (make-score 0 1 0))))
