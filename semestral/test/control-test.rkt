#lang racket

(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "../control.rkt"
         "../const+aux.rkt"
         "../tetriminos.rkt")


;; control:
;; hand:
(define HAND-CTRL-EX1
  (block-placement L (make-posn 0 -10))) ;; L middle
(define HAND-CTRL-EX2
  (block-placement O (make-posn -4 -10))) ;; O left corner
(define HAND-CTRL-EX3
  (block-placement I (make-posn +3 -10))) ;; I right corner
(define HAND-CTRL-EX4
  (block-placement O (make-posn 0 -17))) ;; O where it could be blocked by blocks
;; designed with move-down in mind:
(define HAND-CTRL-EX5
  (block-placement Z (make-posn -3 -20))) ;; Z left bottom cant move down
(define HAND-CTRL-EX6
  (block-placement S (make-posn -3 -19))) ;; left one above bottom
(define HAND-CTRL-EX7
  (block-placement T (make-posn 0 -18))) ;; middle two above bottom
;; blocks:
(define BLOCKS-CTRL-EX1
  (list)) ;; no blocks
(define BLOCKS-CTRL-EX2
  (list (make-block (make-posn 3 11) "orange"))) ;; 1 to the right of left corner
(define BLOCKS-CTRL-EX3
  (list (make-block (make-posn 9 11) "yellow"))) ;; 1 to the left of right corner
(define BLOCKS-CTRL-EX4
  (append (block-placement O (make-posn -2 -17))
          (block-placement O (make-posn 2 -17)))) ;; blocking the HAND-CTR-EX4 from both sides
;; designed with move-down in mind:
(define BLOCKS-CTRL-EX5
  (block-placement I (make-posn -3 -20))) ;; 1 the left bottom
(define BLOCKS-CTRL-EX6
  (list (make-block (make-posn 5 1) "orange")
        (make-block (make-posn 5 2) "orange"))) ;; 2 on top of each other in the middle

;; OK
(define CTRL-L-EX1
  (make-tet HAND-CTRL-EX1 BLOCKS-CTRL-EX1 #t #t)) ;; middle on empty field
(define CTRL-L-EX2
  (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX2 #t #t)) ;; right corner, blocked on the left
;; blocked                    
(define CTRL-L-EX3
  (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX3 #t #t)) ;; blocked by block and wall, slams into block
(define CTRL-L-EX4
  (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX4 #t #t)) ;; blocked by left corner
(define CTRL-L-EX5
  (make-tet HAND-CTRL-EX4 BLOCKS-CTRL-EX4 #t #t)) ;; blocked by block to the left
(define CTRL-L-EX6
  (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX1 #t #t)) ;; blocked by left corner, no blocks around
;; OK
(define CTRL-R-EX1
  (make-tet HAND-CTRL-EX1 BLOCKS-CTRL-EX1 #t #t)) ;; middle on empty field
(define CTRL-R-EX2
  (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX3 #t #t)) ;; left corner block on right corner
;; Blocked
(define CTRL-R-EX3
  (make-tet HAND-CTRL-EX2 BLOCKS-CTRL-EX2 #t #t)) ;;blocked by block and wall slams into block
(define CTRL-R-EX4
  (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX4 #t #t)) ;; blocked by right corner
(define CTRL-R-EX5
  (make-tet HAND-CTRL-EX4 BLOCKS-CTRL-EX4 #t #t)) ;; blocked by block to the right
(define CTRL-R-EX6
  (make-tet HAND-CTRL-EX3 BLOCKS-CTRL-EX1 #t #t)) ;; blocked by right corner, no block around
;; move-down
(define CTRL-D-EX1
  (make-tet HAND-CTRL-EX5 BLOCKS-CTRL-EX1 #t #t)) ;; can't move down because of the floor
(define CTRL-D-EX2
  (make-tet HAND-CTRL-EX6 BLOCKS-CTRL-EX5 #t #t)) ;; can't move down because it is blocked by a block
(define CTRL-D-EX3
  (make-tet HAND-CTRL-EX7 BLOCKS-CTRL-EX6 #t #t)) ;; can't move down because it is blocked by a block


(test/gui
 (test-suite
  "Control"
  (test-suite
   "control"
   (test-suite
    "Blocked by wall"
    (test-equal? "move left blocked by left corner"
               (control CTRL-L-EX4 "left")
               CTRL-L-EX4)
    (test-equal? "move left blocked by left corner on empty field"
               (control CTRL-L-EX6 "left")
               CTRL-L-EX6)
    (test-equal? "move right blocked by right corner"
               (control CTRL-R-EX4 "right")
               CTRL-R-EX4)
    (test-equal? "move right blocked by right corner on empty field"
               (control CTRL-R-EX6 "right")
               CTRL-R-EX6))
   (test-suite
    "Blocked from block/both sides"
    (test-equal? "move left blocked by left corner"
               (control CTRL-L-EX3 "left")
               CTRL-L-EX3)
    (test-equal? "move left blocked by block"
               (control CTRL-L-EX5 "left")
               CTRL-L-EX5)
    (test-equal? "move right blocked by block"
               (control CTRL-R-EX3 "right")
               CTRL-R-EX3)
    (test-equal? "move right blocked by block"
               (control CTRL-R-EX5 "right")
               CTRL-R-EX5)))    
   (test-suite
    "Check left"
    (test-equal? "middle of the field, passes"
               (check-left HAND-CTRL-EX1 BLOCKS-CTRL-EX1)
               #t)
    (test-equal? "right corner, passes"
               (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX2)
               #t)
    (test-equal? "right corner, blocked by block"
               (check-left HAND-CTRL-EX3 BLOCKS-CTRL-EX3)
               #f)
    (test-equal? "left corner, blocked by corner"
               (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX4)
               #f)
    (test-equal? "somewhere, blocked by block"
               (check-left HAND-CTRL-EX4 BLOCKS-CTRL-EX4)
               #f)
    (test-equal? "left corner blocked by corner"
               (check-left HAND-CTRL-EX2 BLOCKS-CTRL-EX1)
               #f))
   (test-suite
    "Check-right"
    (test-equal? "middle of the field, passes"
               (check-right HAND-CTRL-EX1 BLOCKS-CTRL-EX1)
               #t)
    (test-equal? "left corner, passes"
               (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX3)
               #t)
    (test-equal? "left corner, blocked by block"
               (check-right HAND-CTRL-EX2 BLOCKS-CTRL-EX2)
               #f)
    (test-equal? "right corner, blocked by corner"
               (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX4)
               #f)
    (test-equal? "somewhere, blocked by block"
               (check-right HAND-CTRL-EX4 BLOCKS-CTRL-EX4)
               #f)
    (test-equal? "right corner, blocked by corner"
               (check-right HAND-CTRL-EX3 BLOCKS-CTRL-EX1)
               #f))
   (test-suite
    "Move left"
    (test-equal? "L middle"
               (move-left HAND-CTRL-EX1)
               (block-placement HAND-CTRL-EX1 (make-posn -1 0)))
    (test-equal? "I right corner"
               (move-left HAND-CTRL-EX3)
               (block-placement HAND-CTRL-EX3 (make-posn -1 0)))
    (test-equal? "O middle"
               (move-left HAND-CTRL-EX4)
               (block-placement HAND-CTRL-EX4 (make-posn -1 0))))
   (test-suite
    "Move right"
    (test-equal? "L middle"
               (move-right HAND-CTRL-EX1)
               (block-placement HAND-CTRL-EX1 (make-posn 1 0)))
    (test-equal? "O left corner"
               (move-right HAND-CTRL-EX2)
               (block-placement HAND-CTRL-EX2 (make-posn 1 0)))
    (test-equal? "O middle"
               (move-right HAND-CTRL-EX4)
               (block-placement HAND-CTRL-EX4 (make-posn 1 0))))
    "is-at-x?"
    (test-equal? "I, 10"
                 (is-at-x? I 4)
                 #t)
    (test-equal? "O right, 10"
                 (is-at-x? (block-placement O (make-posn 4 -15)) 10)
                 #t)
    (test-equal? "I right, 1"
                 (is-at-x? (block-placement I (make-posn 3 -5)) 1)
                 #f))
   (test-suite
    "rotate-cw-90"
    (test-equal? "2 2"
                 (rotate-cw-90 (make-posn 2 2))
                 (make-posn 2 -2))
    (test-equal? "15 -25"
                 (rotate-cw-90 (make-posn 15 -25))
                 (make-posn -25 -15)))
   (test-suite
    "translation"
    (test-equal? "10 10, 8 12"
                 (translation (make-posn 10 10) (make-posn 8 12))
                 (make-posn -2 2))
    (test-equal? "7 8, 8 12"
                 (translation (make-posn 7 8) (make-posn 8 12))
                 (make-posn 1 4))
    (test-equal? "0 0, 2 4"
                 (translation (make-posn 0 0) (make-posn 2 4))
                 (make-posn 2 4)))
   (test-suite
    "rotate!"
    (test-equal? "Z(0 -10)"
                 (rotate!
                  (block-placement Z (make-posn 0 -10))
                  (make-block (make-posn 5 11) "red"))
                 (list (make-block (make-posn 5 11) "red")
                       (make-block (make-posn 6 12) "red")
                       (make-block (make-posn 6 11) "red")                            
                       (make-block (make-posn 5 10) "red")))
    (test-equal? "J(+1 -15)"
                 (rotate!
                  (block-placement J (make-posn 1 -15))
                  (make-block (make-posn 6 6) "dark blue"))
                 (list (make-block (make-posn 6 6) "dark blue")
                       (make-block (make-posn 7 7) "dark blue")
                       (make-block (make-posn 6 7) "dark blue")
                       (make-block (make-posn 6 5) "dark blue"))))
   (test-suite
    "rotate-I/O!"
   (test-suite
    "Block identifiers"
    (test-suite
     "I"
     (test-suite
      "is-I?"
      (test-equal? "I(+1,-5)"
                   (is-I? (block-placement I (make-posn 1 -5)))
                   #t)
      (test-equal? "S(0 -15)"
                   (is-I? (block-placement S (make-posn 0 -15)))
                   #f)
      (test-equal? "rotated I"
                  (is-I? (rotate! I (make-block (make-posn 5.5 20.5) "light blue")))
                  #t))
     (test-suite
      "all-x?"
      (test-equal? "I"
                   (all-x? I)
                   #f)
      (test-equal? "O"
                   (all-x? O)
                   #f)
      (test-equal? "rotated I"
                  (all-x? (rotate! I (make-block (make-posn 5.5 20.5) "light blue")))
                  #t))
     (test-suite
      "all-y?"
      (test-equal? "I"
                   (all-y? I)
                   #t)
      (test-equal? "T"
                   (all-y? T)
                   #f)
      (test-equal? "rotated I"
                   (all-y? (rotate! I (make-block (make-posn 5.5 20.5) "light blue")))
                   #f)))
    (test-suite
     "O"
     (test-suite
      "is-O?"
      (test-equal? "O"
                   (is-O? O)
                   #t)
      (test-equal? "rotated O"
                   (is-O? (rotate! O (make-block (make-posn 5.5 21.5) "yellow")))
                   #t)
      (test-equal? "rotated rotated O(-2, -13)"
                  (is-O? (rotate!
                          (rotate! (block-placement O (make-posn -2 -13))
                                   (make-block (make-posn 3.5 8.5) "yellow"))
                          (make-block (make-posn 3.5 8.5) "yellow")))
                  #t)
      (test-equal? "S"
                   (is-O? S)
                   #f)
      (test-equal? "T"
                   (is-O? T)
                   #f)
      (test-equal? "L"
                   (is-O? L)
                   #f))
     (test-suite
      "rotate-O!"
      (test-equal? "O"
                   (rotate-O! O)
                   (list (make-block (make-posn 6 22) "yellow")
                         (make-block (make-posn 6 21) "yellow")
                         (make-block (make-posn 5 22) "yellow")
                         (make-block (make-posn 5 21) "yellow")))
      (test-equal? "rotated O"
                   (rotate-O! (rotate-O! O))
                   (list (make-block (make-posn 6 21) "yellow")
                         (make-block (make-posn 5 21) "yellow")
                         (make-block (make-posn 6 22) "yellow")
                         (make-block (make-posn 5 22) "yellow"))))
     (test-suite
      "pos-eq?"
      (test-equal? "5 5; 6 5"
                   (pos-eq? (make-block (make-posn 5 5) "yellow")
                            (make-block (make-posn 6 5) "yellow"))
                   #f)
      (test-equal? "5 5; 5 5"
                   (pos-eq? (make-block (make-posn 5 5) "yellow")
                            (make-block (make-posn 5 5) "yellow"))
                   #t))))))
                                   
                   
                 
                 
                 
    
               
    
               




       
