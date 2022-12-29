#lang racket

(require "const+aux.rkt"
         lang/posn
         "tock.rkt"
         (only-in "draw.rkt" ghost-block-pos))
(provide (all-defined-out))


;; Clock Ke -> Clock
;; Take care of resolving pause and also resets clock-tick
;; if tetrimino moved down because of player action
(define (control-pause clock ke)
  (cond
    [(string=? ke "p")
     (make-clock (clock-tick clock)
                 (clock-tet clock)
                 (not (clock-pause clock)))]
    [(clock-pause clock) clock] ;; = true
    [else (make-clock (cond
                        [(or (string=? ke "down") (string=? ke "\r")) 0]
                        [else (clock-tick clock)])
                      (control (clock-tet clock) ke)
                      (clock-pause clock)
                      (clock-music clock))]))
                   

;; Tet Ke -> Tet
;; moves the tetrimono left or right if there is nothing blocking it's path
;; 1. "left" - moves the tetrimono one left on the grid, does not move if it is at the left border or there is another tetrimono one to the left
;; 2. "right" - moves the tetrimono one right on the grid, does not move if it is a the right border or there is another tetrimono one to the right
;; 3. "down" - moves the tetrimono to the lowest viable position
;; 4. "up" - rotates the tetrimono
(define (control tet ke)
  (cond
    [(and
      (string=? ke "left")
      (check-left (tet-hand tet) (tet-blocks tet)))
     (make-tet (move-left (tet-hand tet)) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [(and
      (string=? ke "right")
      (check-right (tet-hand tet) (tet-blocks tet)))
     (make-tet (move-right (tet-hand tet)) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [(string=? ke "down")
     (move-down (tet-hand tet) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [(string=? ke "up")
     (rotate-event tet)]
    [(string=? ke "\r")
     (quick-fall (tet-hand tet) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [else tet]))

;; Tet -> Tet
;; rotates the tetrimono
;; either in-place, or it is "kicked" left/right/up
(define (rotate-event tet)
  (make-tet
   (cond
     [(rotate? (tet-hand tet) (tet-blocks tet))
      (rotate-me (tet-hand tet))]
     [(rotate? (block-placement (tet-hand tet) (make-posn 1 0)) (tet-blocks tet))
      (rotate-me (block-placement (tet-hand tet) (make-posn 1 0)))]
     [(rotate? (block-placement (tet-hand tet) (make-posn -1 0)) (tet-blocks tet))
      (rotate-me (block-placement (tet-hand tet) (make-posn -1 0)))]
     [(rotate? (block-placement (tet-hand tet) (make-posn 0 1)) (tet-blocks tet))
      (rotate-me (block-placement (tet-hand tet) (make-posn 0 1)))]
     [(rotate? (block-placement (tet-hand tet) (make-posn 0 2)) (tet-blocks tet))
      (rotate-me (block-placement (tet-hand tet) (make-posn 0 2)))]
     [(rotate? (block-placement (tet-hand tet) (make-posn 2 0)) (tet-blocks tet))
      (rotate-me (block-placement (tet-hand tet) (make-posn 2 0)))] ;; case specially for I
     [else (tet-hand tet)])
   (tet-blocks tet) (tet-bag tet) (tet-score tet)))

;; Posn(tet-hand) ListOfPosn(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x-1,y)or if they are on the border
(define (check-left hand blocks)
  (cond
    [(is-at-x? hand 1) #f]
    [(aux-blocked? (move-left hand) blocks) #f]
    [else #t]))

;; ListOfBlocks Num -> Bool
;; checks if any block of Hand is at x
(define (is-at-x? hand x)
  (ormap (lambda (block)
           (= (posn-x (block-posn block)) x))
         hand))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; returns false, if any of the tet-blocks are tet-hand(x+1,y)
(define (check-right hand blocks)
  (cond
    [(is-at-x? hand 10) #f]
    [(aux-blocked? (move-right hand) blocks) #f]
    [else #t]))

;; Posn(hand) -> Posn
;; substracts 1 from posn-x and moves the block left
(define (move-left hand)
  (block-placement hand (make-posn -1 0)))

;; Posn(hand) -> Posn
;; adds 1 to posn-x and moves the block right
(define (move-right hand)
  (block-placement hand (make-posn 1 0)))

;; Posn(tet-hand) ListOf(Posn)(tet-blocks)-> Tet
;; Moves the tet-hand block one down if not blocked, if so sticks it to the tet-blocks
;; 1. There are no blocks on the x coordinate -> (x y - 1)
;; 2. There are some blocks -> places the tetrimono
(define (move-down hand blocks bag score)
  (cond
    [(is-blocked? hand blocks) (block-row hand blocks bag score)] ; tock responsibility
    [else (make-tet
           (block-placement hand (make-posn 0 -1))
           blocks bag score)]))

;; Posn -> Posn
;; rotates a block by 90° clockwise around (0,0)
(define (rotate-cw-90 posn)
  (make-posn  (posn-y posn) 
              (* (posn-x posn) -1)))

;; ListOfBlocks Num -> Bool
;; checks if any block of Hand is at y
(define (is-at-y? hand y)
  (ormap (lambda (block)
           (= (posn-y (block-posn block)) y))
         hand))

;; Posn Posn -> Posn
;; translates a position, so that the origin pos is (0,0)
(define (translation origin pos)
  (make-posn (- (posn-x pos) (posn-x origin))
             (- (posn-y pos) (posn-y origin))))

;; Hand Blocks -> Bool
;; checks if a rotation is possible without collision
(define (rotate? hand blocks)
  (cond
    [(is-O? hand) #f]
    [(or (aux-blocked? (rotate-me hand) blocks)
         (is-at-x? (rotate-me hand) 11)
         (is-at-x? (rotate-me hand) 0)
         (is-at-y? (rotate-me hand) 0)) #f]
    [else #t]))

;; Hand -> Bool
;; checks if a block is I
(define (is-I? hand)
  (cond
    [(or (all-x? hand) (all-y? hand)) #t]
    [else #f]))

;; Hand -> Bool
;; checks if all blocks are on same x - verical
(define (all-x? hand)
  (andmap (lambda (block)
            (= (posn-x (block-posn (first hand))) (posn-x (block-posn block))))
          hand))
  

;; Hand -> Bool
;; check if all blocks are on same y - horizontal
(define (all-y? hand)
  (andmap (lambda (block)
            (= (posn-y (block-posn (first hand))) (posn-y (block-posn block))))
          hand))

;; Hand -> Bool
;; check if a block is an O. Abuses the fact that O-s never rotate
;; therefore their structure is always perserved
(define (is-O? hand)
  (let
      ([left-top-pos (block-posn (first hand))]
       [right-top-pos (block-posn (second hand))]
       [left-bottom-pos (block-posn (third hand))]
       [right-bottom-pos (block-posn (fourth hand))])
    (and (= (posn-y left-top-pos) (posn-y right-top-pos))
         (= (posn-x left-top-pos) (posn-x left-bottom-pos))
         (= (posn-x right-top-pos) (posn-x right-bottom-pos))
         (= (posn-y left-bottom-pos) (posn-y right-bottom-pos)))))
       
;; Block Block -> Bool
;; returns true if posns of blocks are equal
(define (pos-eq? block1 block2)
  (and (= (posn-x (block-posn block1))
          (posn-x (block-posn block2)))
       (= (posn-y (block-posn block1))
          (posn-y (block-posn block2)))))

;; Hand -> Hand
;; rotates a hand cw by 90° based on a origin block
;; moves the tetrimino, so the origin is at (0, 0)
;; and rotates the rest around the origin
(define (rotate! hand origin)
  (block-placement (map (lambda (block)
                          (make-block
                           (rotate-cw-90 (translation
                                          (block-posn origin)
                                          (block-posn block)))
                           (block-col block)))
                        hand)
                   (block-posn origin)))

;; Hand -> Hand
;; rotates an I
;; I is special in that it's rotation origin is not center
;; of one of it's blocks, therefore the complex origin-creation
;; Super rotational system for I:
;; https://strategywiki.org/wiki/File:Tetris_rotation_super.png
(define (rotate-I! hand)
  (cond [(all-y? hand)
         (rotate! hand (make-block
                        (make-posn
                         (half (+ (posn-x (block-posn (second hand)))
                                  (posn-x (block-posn (third hand)))))
                         (- (posn-y (block-posn (second hand))) 0.5))
                        (block-col (first hand))))]
        [(all-x? hand)
         (block-placement (rotate! hand (make-block
                                         (make-posn
                                          (+ 0.5 (posn-x (block-posn (second hand))))
                                          (half (+ (posn-y (block-posn (second hand)))
                                                   (posn-y (block-posn (third hand))))))
                                         (block-col (first hand))))
                          (make-posn -1 0))]))

;; Hand -> Hand
;; rotates a tetrimino. the tetriminos are defined in a way
;; so the first block is ideal for rotating, exception being I
(define (rotate-me hand)
  (cond
    [(is-O? hand) hand]
    [(is-I? hand) (rotate-I! hand)]
    [else (rotate! hand (first hand))]))

;; Hand Blocks -> Blocks
;; pushes a tetrimino as far down as it can
(define (quick-fall hand blocks bag score)
  (cond
    [(is-blocked? hand blocks) (block-row hand blocks bag score)] ;; responsibility of tock
    [else (block-row (ghost-block-pos hand blocks) blocks bag score)])) ;; responsibility of tock


