#lang racket

(require lang/posn 2htdp/image)
(provide (all-defined-out))

;; ================ DATA DEFINITIONS: ================

(define-struct block [posn col]
  #:transparent)
;; Block is a Structure
;; (make-block (Posn, String)
;; Implementation:
;; Posn describes the position of a block
;; Col describes the color of the block
;; col is an enumeration:
;; [light blue, dark blue, orange, yellow, green, red, magenta]

(define-struct tet [hand blocks bag score]
  #:transparent)
;; Tet is a Structure
;; (make-tet (ListOfBlock, ListOfBlock, ListOfListOfBlock, Num)
;; Implementation:
;; tet-hand describes where the current falling controllable tetrimino is located and its color
;; tet-blocks describes where the other already fallen blocks are + their color
;; tet-bag describes what the next tetriminos are
;; tet-score describes the current score of the player

(define-struct score [score level lines-cleared]
  #:transparent)
;; Score is a Structure
;; (make-score (Num Num Num)
;; Implementation:
;; score-score describes the current score
;; score-level describest the current level
;; score-lines-cleared describes the number of lines-cleared

;;================ Quality of life aux. functions: ================
;; debatable, if tests are valid here, still should fit in somehow ̄\_(ツ)_/̄

;; Num -> Num
;; halves the given number
(define (half num)
  (/ num 2))

;; used for painting grid
(define PEN
  (make-pen "gray" 1 "solid" "round" "round"))

;; Num Image -> Image
;; An auxiliary function for drawing x portion of the grid
(define (aux-grid-x num img)
  (cond
    [(= num X-LINES) img]
    [else (aux-grid-x
           (+ num 1)
           (add-line
            img
            0
            (aux-x-position num)
            (image-width BORDER)
            (aux-x-position num)
            PEN
             ))]))

;; Num Image -> Image
;; An auxiliary function for drawing y portion of the grid
(define (aux-grid-y num img)
  (cond
    [(= num Y-LINES) img]
    [else (aux-grid-y
           (+ num 1)
           (add-line
            img
            (aux-y-position num)
            0
            (aux-y-position num)
            (image-height BORDER)
            PEN
             ))]))

;; Num -> Num
;; Auxiliary for aux-grid-x for calculating the actual start and end point of the line
(define (aux-x-position num)
  (* (+ SHIVER num) CUBE-LENGTH))

;; Num -> Num
;; Auxiliary for aux-grid-y for calculating the actual start and end point of the line
(define (aux-y-position num)
  (* num CUBE-LENGTH))

;; ================ Mathematical constants (mostly): ================
(define CUBE-LENGTH 20)
(define SCENE-WIDTH-INDEX 20)
(define SCENE-HEIGHT-INDEX 30)
(define SCENE-WIDTH (* SCENE-WIDTH-INDEX CUBE-LENGTH))
(define SCENE-HEIGHT (* SCENE-HEIGHT-INDEX CUBE-LENGTH))
(define HALF-SCENE-WIDTH (half SCENE-WIDTH))
(define HALF-SCENE-HEIGHT (half SCENE-HEIGHT)) 
(define MTSC (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define SHIVER 0.4)

(define X-OFFSET (* 4.5 CUBE-LENGTH)) ;; breaks when SCENE-WIDTH//HEIGHT-INDEX is changed, careful!!! 
(define Y-OFFSET (* 25.7 CUBE-LENGTH)) ;; breaks when SCENE-WIDTH//HEIGHT-INDEX is changed, careful!!! 
(define MID-HEIGTH (- Y-OFFSET (- SCENE-HEIGHT Y-OFFSET) CUBE-LENGTH))

;; these are just for drawing purposes and they mean how many || are there to each axis in a grid
(define X-LINES 20)
(define Y-LINES 10)



(define WHITE-SPACE-Y-POS (half (* 5.5 CUBE-LENGTH)))

;; ================ Graphical constants (mostly): ================

(define (draw-block col)
  (square CUBE-LENGTH "solid" col))
(define BORDER (rectangle (* Y-LINES CUBE-LENGTH) (* (+ X-LINES SHIVER) CUBE-LENGTH) "outline" "black"))
(define GRID-X
  (aux-grid-x 0 BORDER))
(define GRID-Y
  (aux-grid-y 0 BORDER))
(define GRID (aux-grid-x 0 (aux-grid-y 1 BORDER)))


(define TOP-SPACE (rectangle SCENE-WIDTH (half (- SCENE-HEIGHT (image-height GRID))) "solid" "blue"))
(define TETRIS-SPACE (overlay
                      (text "Tetris" (* 3.6 CUBE-LENGTH) "black")
                      TOP-SPACE))
(define TETRIS-SPACE-FINAL
  (place-images
   (list
    (text "Score" CUBE-LENGTH "white")
    (text "Level" CUBE-LENGTH "white")
    (rectangle (* 3.75 CUBE-LENGTH) (* 3.5 CUBE-LENGTH) "solid" "black")
    (rectangle (* 3.75 CUBE-LENGTH) (* 3.5 CUBE-LENGTH) "solid" "black"))         
   (list
    (make-posn (* 17.5 CUBE-LENGTH) (* 0.75 CUBE-LENGTH))
    (make-posn (* 2.5 CUBE-LENGTH) (* 0.75 CUBE-LENGTH))
    (make-posn (* 17.5 CUBE-LENGTH) (* 1.5 CUBE-LENGTH))
    (make-posn (* 2.5 CUBE-LENGTH) (* 1.5 CUBE-LENGTH)))
   TETRIS-SPACE))

(define PREVIEW-WINDOW (overlay
                        (rectangle (* 15 CUBE-LENGTH) (* 3.5 CUBE-LENGTH) "outline" "black")
                        (rectangle (* 15 CUBE-LENGTH) (* 3.5 CUBE-LENGTH) "solid" "white")))


;;================ Pictures: ================
;; Basic layout:

(define ADV-MTSC (above (beside (rectangle (* 5 CUBE-LENGTH) (image-height GRID) "solid" "light blue")
                                (rectangle (* 10 CUBE-LENGTH) (image-height GRID) "solid" "white")
                                (rectangle (* 5 CUBE-LENGTH) (image-height GRID) "solid" "light blue"))
                        TOP-SPACE))


(define PLACED-MTSC (place-image ADV-MTSC HALF-SCENE-WIDTH (+ HALF-SCENE-HEIGHT (half (image-height TOP-SPACE))) MTSC))

(define PLACED-MTSC-PREVIEW (place-image
                             PREVIEW-WINDOW
                             HALF-SCENE-WIDTH
                             (* 27.5 CUBE-LENGTH)
                             PLACED-MTSC))

(define FINAL-LAYOUT (place-image GRID HALF-SCENE-WIDTH HALF-SCENE-HEIGHT PLACED-MTSC))

;; Blocks, Posn -> Blocks
;; moves a tetrimino by posn-x and posn-y
(define (block-placement blocks posn)
  (map (lambda (block)
         (make-block (make-posn
                      (+ (posn-x (block-posn block)) (posn-x posn))
                      (+ (posn-y (block-posn block)) (posn-y posn)))
                      (block-col block)))
       blocks))

;;================ Last frame: ================
(define LAST-MTSC
  (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))
(define GAME-OVER
  (text "GAME OVER" (* 3 CUBE-LENGTH) "white"))


                   

