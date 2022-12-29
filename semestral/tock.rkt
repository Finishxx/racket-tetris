#lang racket

(require 2htdp/image
         "const+aux.rkt"
         lang/posn
         "tetriminos.rkt"
         rsound)
(provide (all-defined-out))

;; Plays music
(define (play-music music)
  (begin
    (define NEW-START (pstream-current-frame STREAM))
    (define NEW-END (+ NEW-START MUSIC-LENGTH))
    (pstream-queue STREAM TETRIS-MUSIC NEW-START)
    NEW-END))

;; Decides if it is the right time to play music, if so returns its end-frame
(define (tock-music music)
  (cond
    [(<= music (pstream-current-frame STREAM))
     (play-music music)]
    [else music]))

;; Clock -> Clock
;; if pause, we skip, else we do
(define (tock-pause clock)
  (cond
    [(clock-pause clock) clock] ;; = true
    [(>= (clock-tick clock) (get-ticks-for-tock (score-level (tet-score (clock-tet clock)))))
     (make-clock 0
                 (tock (clock-tet clock))
                 (clock-pause clock)
                 (tock-music (clock-music clock)))]
    [else (make-clock (+ (clock-tick clock) 1)
                        (clock-tet clock)
                        (clock-pause clock)
                        (tock-music (clock-music clock)))]))

;; Tet -> Tet
;; If tetrimino can't fall, we call block-row, else we let it fall by one
(define (tock tet)
  (cond
    [(is-blocked? (tet-hand tet) (tet-blocks tet)) 
     (block-row (tet-hand tet) (tet-blocks tet) (tet-bag tet) (tet-score tet))]
    [else
     (make-tet (fall (tet-hand tet)) (tet-blocks tet) (tet-bag tet) (tet-score tet))])) 

;; Posn(tet-hand) ListOf(Posn)(tet-blocks) -> Bool
;; Returns true if either one of these is true:
;; 1. The block is at position (x 1)
;; 2. The block is directly above any of the blocks -> tet-hand(x y+1) tet-blocks(x y)
;; Otherwise returns false
(define (is-blocked? hand blocks) 
  (cond
    [(is-bottom? hand) #t]
    [(aux-blocked? (block-placement hand (make-posn 0 -1)) blocks) #t]
    [else #f])) 

;; Hand -> Bool
;; checks if a tetrimino is at the bottom
(define (is-bottom? hand)
  (ormap (lambda (block)
           (= (posn-y (block-posn block)) 1))
         hand)) 

;; ListOfBlocks ListOfBlocks -> Bool
;; checks if any block in hand is equal in position to any block in blocks
(define (aux-blocked? hand blocks)
  (cond
    [(empty? blocks) #f]
    [(or (posn-equal? hand (first blocks))
         (aux-blocked? hand (rest blocks)))]
    [else #f])) 


;; ListOfBlocks Block -> Bool
;; compares the posn of tetrimino and a block
;; to return #t if they are equal or #f if they are not
(define (posn-equal? hand blocks)
  (ormap (lambda (block)
           (and (= (posn-x (block-posn block)) (posn-x (block-posn blocks)))
                (= (posn-y (block-posn block)) (posn-y (block-posn blocks)))))
         hand))

;; Hand Blocks Bag Score -> Tet
;; spawns a new block, depending on if any rows are full, cleares them,
;; updates bag and score
(define (block-row hand blocks bag score)
  (let ([new-blocks
        (cond
          [(clear-row? (append hand blocks) 0)
           (clear-row! (append hand blocks) (which-row (append hand blocks) 1))]
          [else (block! hand blocks)])])
  (make-tet (spawn-block bag)
            new-blocks
            (update-bag bag)
            (update-score (append hand blocks) new-blocks score))))

;; Bag -> ListOfBlocks
;; spawns a new tetrimino, the first in the bag
(define (spawn-block bag)
  (first bag)) 

;; ListOf(posn)(blocks) Num -> Bool
;; checks if a row of 10 is cleared
;; checks the row on the hand-x if it is full
(define (clear-row? blocks y)
  (cond
    [(is-ten? blocks y) #t]
    [(>= y 20) #f]
    [else (clear-row? blocks (+ y 1))]))

;; ListOfBlocks Num -> ListOfBlocks
;; clears a target row and repeats calling itself with new state
(define (clear-row! blocks target)
  (cond
    [(number? target)
     (let ([new-blocks (dirty-work blocks target)])
       (clear-row! new-blocks (which-row new-blocks 1)))]
    [else blocks]))

;; ListOfBlocks Num -> ListOfBlocks
;; kills a target row and moves the blocks above it 1 down
(define (dirty-work blocks target)
  (move-row (kill-row blocks (select-row blocks target)) target))

;; ListOfBlocks Num -> Num
;; returns y pos of the first row from bottom that is full
(define (which-row blocks y)
  (cond
    [(is-ten? blocks y) y]
    [(>= y 21) #f]
    [else (which-row blocks (+ y 1))])) 

;; ListOfBlocks Num -> Bool
;; checks if there is 10 blocks on one row given the blocks and row num
(define (is-ten? blocks y)
  (= 10 (length (select-row blocks y))))

;; ListOfBlocks -> ListOfBlocks
;; changes the value for tet-hand posn(x, y-1)
(define (fall hand)
  (block-placement hand (make-posn 0 -1)))

;; Block ListOfBlocks -> ListOfBlocks
;; appends the value of tet-hand to list tet-blocks and returns new tet-blocks
(define (block! hand blocks)
  (append hand blocks))
  
;; ListOfBlocks Num -> ListOfBlocks
;; selects all blocks that are in y row from blocks
(define (select-row blocks y)
  (filter (lambda (block)
            (= (posn-y (block-posn block)) y))
          blocks))


;; ListOfBlocks ListOfBlocks Num -> ListOfBlocks
;; select all rows above given y
(define (select-above blocks rows y)
  (cond [(< y 22) (select-above blocks (append rows (select-row blocks y)) (+ 1 y))]
        [else rows]))

;; ListOfBlocks ListOfBlocks Num -> ListOfBlocks
;; select all rows below given y
(define (select-below blocks rows y)
  (cond [(> y 0) (select-below blocks (append rows (select-row blocks y)) (- y 1))]
        [else rows]))


;; ListOfBlocks ListOfBlocks -> ListOfBlocks
;; given a row deletets all of it's members from blocks
(define (kill-row blocks row)
  (remove* row blocks))

;; ListOfBlocks Num -> ListOfBlocks
;; given a list of blocks moves all above the y down
;; selects the row above the deleted one, moves it down
;; and goes on untill row 21
(define (move-row blocks y)
  (append (select-below blocks (list) y) (fall-down (select-above blocks (list) y))))

;; ListOfBlocks
;; Given a row of blocks moves them down by one
(define (fall-down blocks)
  (map (lambda (arg)
         (make-block (make-posn (posn-x (block-posn arg)) (- (posn-y (block-posn arg)) 1)) (block-col arg)))
         blocks))

;; Bag -> Bag
;; checks if bag needs to be updated, if so append a new random bag
;; to the old one
(define (update-bag bag)
  (cond
    [(< (length bag) 4) (append (rest bag) (shuffle DEFAULT-BAG))]
    [else (rest bag)])) 

;; ListOfBlocks ListOfBlocks Score -> Score
;; calculates if any rows have been cleared, updates score if so
(define (update-score blocks new-blocks score)
  (let ([cleared-rows (/ (- (length blocks) (length new-blocks)) Y-LINES)])
    (cond
      [(> cleared-rows 0)
       (update-score!
        cleared-rows
        (score-score score)
        (score-level score)
        (score-lines-cleared score))]
      [else score])))

;; Num Score Level Lines-cleared -> Score
;; updates score, level, lines-cleared
(define (update-score! cleared-rows score level lines-cleared)
  (make-score
   (new-score score level cleared-rows)
   (new-level (+ lines-cleared cleared-rows))
   (+ lines-cleared cleared-rows)))

;; Num Num -> Num
;; returns a new score based on current level and number of lines cleared
(define (new-score score level lines-cleared)
  (cond
    [(= lines-cleared 1) (+ score (* level 100))]
    [(= lines-cleared 2) (+ score (* level 300))]
    [(= lines-cleared 3) (+ score (* level 500))]
    ((= lines-cleared 4) (+ score (* level 800)))))

;; Num -> Num
;; returns the current level based on blocks cleared so far
(define (new-level lines-cleared)
  (cond
    [(< lines-cleared 11) 1]
    [(< lines-cleared 31) 2]
    [(< lines-cleared 61) 3]
    [(< lines-cleared 101) 4]
    [(< lines-cleared 151) 5]
    [(< lines-cleared 211) 6]
    [(< lines-cleared 281) 7]
    [(< lines-cleared 361) 8]
    [(< lines-cleared 451) 9]
    [(< lines-cleared 551) 10]
    [else (level-over-10 (- lines-cleared 551))]))

;; Num -> Num
;; returns how many frames before tock is called
;; values from https://harddrop.com/wiki/Tetris_Worlds
(define (get-ticks-for-tock level)
  (cond
    [(= level 1) (round (/ 1 0.01667))]
    [(= level 2) (round (/ 1 0.021017 ))]
    [(= level 3) (round (/ 1 0.026977 ))]
    [(= level 4) (round (/ 1 0.035256 ))]
    [(= level 5) (round (/ 1 0.04693 ))]
    [(= level 6) (round (/ 1 0.06361 ))]
    [(= level 7) (round (/ 1 0.0879 ))]
    [(= level 8) (round (/ 1 0.1236 ))]
    [(= level 9) (round (/ 1 0.1775 ))]
    [(= level 10) (round (/ 1 0.2598 ))]
    [(= level 11) (round (/ 1 0.388 ))]
    [(= level 12) 2]
    [else 1]))

;; Num -> Num
;; if a player reaches level 10+ all next levels are after 100 cleared lines
(define (level-over-10 lines-cleared)
  (+ (quotient lines-cleared 100) 10))

;; Level -> Num
;; sets the clock speed depending on the level
(define (tick-control level)
  (cond
    [(<= level 5) (tick-control-below-5 level)]
    [(<= level 10) (tick-control-below-10 (- level 5))]
    [else 0.11]))

(define (tick-control-below-5 level)
  (- 1 (* 0.1 level)))

(define (tick-control-below-10 level)
  (- 0.5 (* 0.075 level)))


