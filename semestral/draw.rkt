#lang racket
(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         "tock.rkt")
(provide (all-defined-out))


(define (draw-music music)
  (draw-pause (music-clock music)))

;; Clock -> Img
;; if the game is paused adds to the drawing a PAUSE text
(define (draw-pause clock)
  (cond
    [(clock-pause clock) ;; = true
     (place-image PAUSE HALF-SCENE-WIDTH HALF-SCENE-HEIGHT (draw (clock-tet clock)))]
    [(draw (clock-tet clock))]))

;; order first->last: preview, ghost-blocks, blocks, LAYOUT, score, level
(define (draw tet)
  (draw-level
   (score-level (tet-score tet))
   (draw-score
    (score-score (tet-score tet))
    (place-images
     (list TETRIS-SPACE-FINAL
           GRID)
     (list (make-posn HALF-SCENE-WIDTH
                      (half (image-height TETRIS-SPACE-FINAL)))
           (make-posn HALF-SCENE-WIDTH
                      HALF-SCENE-HEIGHT))
     (draw-blocks (append (tet-hand tet)
                          (tet-blocks tet))
                  (ghost-block-draw (tet-hand tet)
                                    (tet-blocks tet)
                                    (block-preview (tet-bag tet) PLACED-MTSC-PREVIEW)))))))

;; blocks bckg -> Image
;; draws blocks onto bckg
(define (draw-blocks blocks bckg)
  (place-images
   (block-list (list-col blocks))
   (schematic->actual (blocks->posn blocks))
   bckg))

;; ListOfBlocks -> ListOfPosn
;; maps each block into it's posn
(define (blocks->posn blocks)
  (map (lambda (block)
         (block-posn block))
         blocks))

;; ListOf(Posn) -> ListOf(Posn)
;; converts the schematic description of posn on the board to actual applicable values
(define (schematic->actual pos)
  (map (lambda (posit)
         (make-posn (inexact->exact (+ X-OFFSET (* (posn-x posit) CUBE-LENGTH)))
                    (inexact->exact (- Y-OFFSET (* (posn-y posit) CUBE-LENGTH))))) pos))

;; ListOfBlocks -> ListOfColor
;; maps each block into it's color
(define (list-col blocks)
  (map (lambda (block)
         (block-col block))
       blocks))

;; ListOfCol -> ListOfImages
;; Creates a list of blocks from given list of colors in order
(define (block-list col)
  (map (lambda (col)
         (square CUBE-LENGTH "solid" col))
       col))

;; ListOfBlocks, Image -> Image
;; draws given blocks into image with border. First draws blocks, then add borders
(define (draw-blocks-preview blocks bckg)
  (let ([block-positions (schematic->actual (blocks->posn blocks))])
    (place-images
     (map (lambda (dummy)
            (square CUBE-LENGTH "outline" "black"))
          blocks)
     block-positions
     (place-images
      (block-list (list-col blocks))
      block-positions
      bckg))))

;; Bag -> Img
;; renders block preview
(define (block-preview bag img)
  (draw-blocks-preview (preview-pos (take bag 3)) img))

;; ListOfBlocks -> ListOfBlocks
;; moves the tetriminos to preview positions
(define (preview-pos blocks)
  (append (preview-y-pos (preview-x-pos (first blocks) -5))
          (preview-y-pos (second blocks))
          (preview-y-pos (preview-x-pos (third blocks) 5))))

;; ListOfBlocks -> ListOfBlocks
;; sets the y pos of a set of blocks to preview compatible level
;; uses schematic values of the blocks in tetriminos.rkt
(define (preview-y-pos blocks)
  (map (lambda (block)
         (make-block (make-posn (posn-x (block-posn block))
                                (- (posn-y (block-posn block)) 23.25))
                     (block-col block)))                               
       blocks))

;; ListOfBlocks Num -> ListOfBlocks
;; changes the x pos of a list of blocks by Num
(define (preview-x-pos blocks i)
  (map (lambda (block)
         (make-block (make-posn (+ (posn-x (block-posn block)) i)
                                (posn-y (block-posn block)))
                     (block-col block)))
         blocks))

;; Num -> Img
;; draws score
(define (draw-score score img)
  (place-image (text (number->string score) CUBE-LENGTH "white")
               (* 17.5 CUBE-LENGTH)
               (* 2.5 CUBE-LENGTH)
               img))

;; Num -> Img
;; draws level
(define (draw-level level img)
  (place-image (text (number->string level) (* 1.5 CUBE-LENGTH) "white")
               (* 2.5 CUBE-LENGTH)
               (* 2.5 CUBE-LENGTH)
               img))

;; Hand Blocks -> ListOfPosn
;; checks every y pos starting from y pos of first block
;; if is-blocked? returns the current hand
(define (ghost-block-pos hand blocks)
  (cond
    [(is-blocked? hand blocks) hand]
    [else (ghost-block-pos (block-placement hand (make-posn 0 -1)) blocks)]))

;; Hand String -> Hand
;; returns a list of col blocks, to draw ghost block with
(define (ghost-block-col hand col)
  (map (lambda (block)
         (make-block (block-posn block) col))
       hand))

;; Hand -> IMG
;; draws a ghost piece
(define (ghost-block-draw hand blocks img)
  (draw-blocks (ghost-block-col (ghost-block-pos hand blocks) "light gray") img))

