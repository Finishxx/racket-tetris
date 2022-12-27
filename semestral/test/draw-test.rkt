#lang racket
(require rackunit
         lang/posn
         rackunit/gui
         2htdp/image
         "tock-test.rkt"
         "../draw.rkt"
         "../const+aux.rkt"
         "../tetriminos.rkt")

(define EX-I (list (make-block (make-posn 3 10) "light blue")
                   (make-block (make-posn 4 10) "light blue")
                   (make-block (make-posn 5 10) "light blue")
                   (make-block (make-posn 6 10) "light blue")))

;; schematic->actual
(define S->A-EX1 (list (make-posn 1 1)
                       (make-posn 10 1)))
(define S->A-EX2 (list (make-posn 1 20)
                       (make-posn 10 20)))
(define S->A-EX3 (list (make-posn 1 10)
                       (make-posn 10 10)))
(define S->A-EX4 (list (make-posn 5 1)
                       (make-posn 5 20)))
;; block-list
(define BL-EX1 1) ;; redo
(define BL-EX2 5)
(define BL-EX3 7)
;; draw-blocks + draw
(define DB-EX1 (make-tet (make-block (make-posn 3 4) "red") S->A-EX1 0 (list)))
(define DB-EX2 (make-tet (make-posn 6 8) S->A-EX2 0 (list I)))
(define DB-EX3 (make-tet (make-posn 2 2) S->A-EX3 0 (list I)))
(define DB-EX4 (make-tet (make-posn 4 15) S->A-EX4 0 (list I)))
;; test-block->posn
(define B-P1 (list (make-block (make-posn 1 2) "orange")
                   (make-block (make-posn 1 5) "orange")
                   (make-block (make-posn 10 5) "orange")))
(define B-P2 (list (make-block (make-posn 15 7) "orange")
                   (make-block (make-posn 20 8) "orange")
                   (make-block (make-posn 8 6) "orange")))
(define B-P3 (list (make-block (make-posn 12 22) "orange")
                   (make-block (make-posn 17 15) "orange")
                   (make-block (make-posn 18 14) "orange")))
;; test-col
(define TC-EX1 (list (make-block (make-posn 1 1) "orange")
                     (make-block (make-posn 1 1) "blue")
                     (make-block (make-posn 1 1) "magenta")))
(define TC-EX2 (list (make-block (make-posn 1 1) "blue")
                     (make-block (make-posn 1 1) "blue")
                     (make-block (make-posn 1 1) "orange")))
(define TC-EX3 (list (make-block (make-posn 1 1) "magenta")
                     (make-block (make-posn 1 1) "orange")
                     (make-block (make-posn 1 1) "blue")))



(test/gui
 (test-suite
  "Schemati->Actual"
  (test-equal? "pos 1 1, pos 10 1"
               (schematic->actual S->A-EX1)
               (list (make-posn 55 247) (make-posn 145 247)))
  (test-equal? "pos 1 20, pos 10 20"
               (schematic->actual S->A-EX2)
               (list (make-posn 55 57) (make-posn 145 57))
               )
  (test-equal? "pos 1 10, pos 10 10"
               (schematic->actual S->A-EX3)
               (list (make-posn 55 157) (make-posn 145 157))
               )
  (test-equal? "pos 5 1, pos 5 20"
               (schematic->actual S->A-EX4)
               (list (make-posn 95 247) (make-posn 95 57))
               ))
 (test-suite
  "blocks->posn"
  (test-equal? "1 2; 1 5; 10 5"
               (blocks->posn B-P1)
               (list (make-posn 1 2) (make-posn 1 5) (make-posn 10 5)))
  (test-equal? "15 7; 20 8; 8 6"
               (blocks->posn B-P2)
               (list (make-posn 15 7) (make-posn 20 8) (make-posn 8 6)))
  (test-equal? "12 22; 17 15; 18 14"
               (blocks->posn B-P3)
               (list (make-posn 12 22) (make-posn 17 15) (make-posn 18 14))))
               
 (test-suite
  "list-col"
  (test-equal? "orange blue magenta"
               (list-col TC-EX1)
               (list "orange" "blue" "magenta"))
  (test-equal? "blue blue orange"
               (list-col TC-EX2)
               (list "blue" "blue" "orange"))
  (test-equal? "magenta orange blue"
               (list-col TC-EX3)
               (list "magenta" "orange" "blue")))
 (test-suite
  "block-list"
  (test-equal? "orange blue magenta"
               (block-list (list-col TC-EX1))
               (list (square CUBE-LENGTH "solid" "orange")
                     (square CUBE-LENGTH "solid" "blue")
                     (square CUBE-LENGTH "solid" "magenta")))
  (test-equal? "blue blue orange"
               (block-list (list-col TC-EX2))
               (list (square CUBE-LENGTH "solid" "blue")
                     (square CUBE-LENGTH "solid" "blue")
                     (square CUBE-LENGTH "solid" "orange")))
  (test-equal? "magenta orange blue"
               (block-list (list-col TC-EX3))
               (list (square CUBE-LENGTH "solid" "magenta")
                     (square CUBE-LENGTH "solid" "orange")
                     (square CUBE-LENGTH "solid" "blue"))))
 (test-suite
  "preview-pos"
  (test-equal? "I J L troll text"
               (preview-pos (list I J L "doesn't get here haha"))
               (list (make-block (make-posn -1 -2.25) "light blue")
                     (make-block (make-posn 0 -2.25) "light blue")
                     (make-block (make-posn 1 -2.25) "light blue")
                     (make-block (make-posn 2 -2.25) "light blue")
                     
                     (make-block (make-posn 4 -1.25) "dark blue")
                     (make-block (make-posn 4 -2.25) "dark blue")
                     (make-block (make-posn 5 -2.25) "dark blue")
                     (make-block (make-posn 6 -2.25) "dark blue")
                     
                     (make-block (make-posn 11 -1.25) "orange")
                     (make-block (make-posn 9 -2.25) "orange")
                     (make-block (make-posn 10 -2.25) "orange")
                     (make-block (make-posn 11 -2.25) "orange")))
  (test-equal? "O S Z"
               (preview-pos (list O S Z))
               (list (make-block (make-posn 0 -1.25) "yellow")
                     (make-block (make-posn 1 -1.25) "yellow")
                     (make-block (make-posn 0 -2.25) "yellow")
                     (make-block (make-posn 1 -2.25) "yellow")
                                          
                     (make-block (make-posn 5 -1.25) "green")
                     (make-block (make-posn 6 -1.25) "green")
                     (make-block (make-posn 4 -2.25) "green")
                     (make-block (make-posn 5 -2.25) "green")
                     
                     (make-block (make-posn 9 -1.25) "red")
                     (make-block (make-posn 10 -1.25) "red")
                     (make-block (make-posn 10 -2.25) "red")
                     (make-block (make-posn 11 -2.25) "red")))
  (test-equal? "S Z T"
              (preview-pos (list S Z T))
               (list (make-block (make-posn 0 -1.25) "green")
                     (make-block (make-posn 1 -1.25) "green")
                     (make-block (make-posn -1 -2.25) "green")
                     (make-block (make-posn 0 -2.25) "green")
                     
                     (make-block (make-posn 4 -1.25) "red")
                     (make-block (make-posn 5 -1.25) "red")
                     (make-block (make-posn 5 -2.25) "red")
                     (make-block (make-posn 6 -2.25) "red")
                     
                     (make-block (make-posn 10 -1.25) "magenta")
                     (make-block (make-posn 9 -2.25) "magenta")
                     (make-block (make-posn 10 -2.25) "magenta")
                     (make-block (make-posn 11 -2.25) "magenta"))))
 (test-suite
  "preview-y-pos"
  (test-equal? "J"
               (preview-y-pos J)
               (list
                (make-block (make-posn 4 -1.25) "dark blue")
                (make-block (make-posn 4 -2.25) "dark blue")
                (make-block (make-posn 5 -2.25) "dark blue")
                (make-block (make-posn 6 -2.25) "dark blue")))
  (test-equal? "T"
               (preview-y-pos T)
               (list
                (make-block (make-posn 5 -1.25) "magenta")
                (make-block (make-posn 4 -2.25) "magenta")
                (make-block (make-posn 5 -2.25) "magenta")
                (make-block (make-posn 6 -2.25) "magenta")))
  (test-equal? "O"
               (preview-y-pos O)
               (list
                (make-block (make-posn 5 -1.25) "yellow")
                (make-block (make-posn 6 -1.25) "yellow")
                (make-block (make-posn 5 -2.25) "yellow")
                (make-block (make-posn 6 -2.25) "yellow"))))
 (test-suite
  "preview-x-pos"
  (test-equal? "Z"
               (preview-x-pos Z -5)
               (list
                (make-block (make-posn -1 22) "red")
                (make-block (make-posn 0 22) "red")
                (make-block (make-posn 0 21) "red")
                (make-block (make-posn 1 21) "red")))
  (test-equal? "L"
               (preview-x-pos L +5)
               (list
                (make-block (make-posn 11 22) "orange")
                (make-block (make-posn 9 21) "orange")
                (make-block (make-posn 10 21) "orange")
                (make-block (make-posn 11 21) "orange")))
  (test-equal? "I"
               (preview-x-pos I -5)
               (list
                (make-block (make-posn -1 21) "light blue")
                (make-block (make-posn 0 21) "light blue")
                (make-block (make-posn 1 21) "light blue")
                (make-block (make-posn 2 21) "light blue"))))
 (test-suite
  "ghost-block-pos"
  (test-equal? "T(0, -10), THREE ROW"
               (ghost-block-pos (block-placement T (make-posn 0 -10)) THREE-ROW)
               (block-placement T (make-posn 0 -17)))
  (test-equal? "O(-4, -15) ONE ROW"
               (ghost-block-pos (block-placement O (make-posn -4 -15)) ONE-ROW)
               (block-placement O (make-posn -4 -19)))
  (test-equal? "I, EMPTY"
               (ghost-block-pos I (list))
               (block-placement I (make-posn 0 -20))))
 (test-suite
  "ghost-block-col"
  (test-equal? "T"
               (ghost-block-col T "gray")
               (list (make-block (make-posn 5 22) "gray")
                     (make-block (make-posn 4 21) "gray")
                     (make-block (make-posn 5 21) "gray")
                     (make-block (make-posn 6 21) "gray")))
  (test-equal? "Z"
               (ghost-block-col Z "gray")
               (list (make-block (make-posn 4 22) "gray")
                     (make-block (make-posn 5 22) "gray")
                     (make-block (make-posn 5 21) "gray")
                     (make-block (make-posn 6 21) "gray")))))
  
 

 



         