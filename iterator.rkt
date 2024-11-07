#lang racket/gui
(require 2htdp/image)
(require 2htdp/universe)

(define max-size 1000)

(define max-grid-length 1)
(define max-grid-height 1)

;; grid is a matrix repersenting each cell
(define grid (list (list 1)))

;; frame
(define frame (new frame% 
                   [label "rule insert binary here (30)"]
                   [width max-size]
                   [height max-size]))
;; canvas
(define canvas (new canvas%
                    [parent frame]
                    [paint-callback (lambda (canvas dc)
                                      (draw-scene dc))]))
;; button panel
(define button-panel 
  (new horizontal-panel% 
       [parent frame]
       [alignment '(center center)]
       [stretchable-height #f]))

;; Make a tick button in the frame
;; generates a new row
(define iter-button 
  (new button% [parent button-panel]
       [label "Tick"]
       ; Callback procedure for a button click:
       (callback (lambda (button event)
                   (set! grid (update-grid grid))
                   (set! max-grid-length (+ max-grid-length 2))
                   (set! max-grid-height (+ max-grid-height 1))
                   (send canvas on-paint)         ; repaint
                   ))))

;; Make a reset button in the frame
;; updates all values to their initial value
(define reset-button 
  (new button% [parent button-panel]
       [label "Reset"]
       ; Callback procedure for a button click:
       (callback (lambda (button event)
                   (set! grid (list (list 1)))
                   (set! max-grid-length 1)
                   (set! max-grid-height 1)
                   (send canvas on-paint)
                   ))))

;; draw scene
(define (draw-scene dc)
  (local [(define w (send canvas get-width))
          (define h (send canvas get-height))
          (define h-spacing (/ w max-grid-length))
          (define v-spacing (/ h max-grid-height))
          (define (vert-lines x n)
            (cond [(= x n) empty]
                  [else (send dc draw-line (* x h-spacing) 0 (* x h-spacing) h)
                        (vert-lines (add1 x) n)]))
          (define (hort-lines y n)
            (cond [(= y n) empty]
                  [else (send dc draw-line 0 (* y v-spacing) w (* y v-spacing) )
                        (hort-lines (add1 y) n)]))
          (define (draw-mesh)
            (vert-lines 1 max-grid-length)
            (hort-lines 1 max-grid-height))
          (define (draw-cell row col)
            (send dc set-brush (make-object brush% "BLACK" 'solid))
            (send dc draw-rectangle
                  (* h-spacing col)
                  (* v-spacing row)
                  h-spacing
                  v-spacing))
          (define (draw-grid)
            (local [(define row 0)
                    (define col 0)
                    ]
              (for ([r grid])
                (set! col 0)
                (for ([item r])
                  (cond [(= 1 item)
                         (draw-cell row col)
                         ;;(write (list row col))
                         ])
                  (set! col (+ 1 col))
                  )
                (set! row (+ 1 row))
                ))
            )
          ]
    (send dc clear)
    (draw-mesh)
    (draw-grid)
    ;;(write (list w h h-spacing v-spacing))
    ;;(printf "\n")
    ))
;;(define (update tick)
;;  (underlay
;;   (empty-scene (* 2 max-size) (* 2 max-size))
;;   (circle (modulo tick max-size) 'solid 'red)))
;;(animate update)
(define (last lst)
  (cond [(empty? (rest lst)) (first lst)]
        [else (last (rest lst))]))

;; (add-0 lst) appends 0 to the left and right of the list
(define (add-0 lst)
  (cons 0 (append lst (list 0))))

;; (update-grid g) increases the size of the grid applying a rule
(define (update-grid g)
  (cond [(empty? (rest g)) (list (add-0 (first g)) (update-grid-helper (append (list 0 0) (last g) (list 0 0))))]
        [else (cons (add-0 (first g)) (update-grid (rest g)))]))

(define (rule30 a b c)
  (cond [(= a 0)
         (cond [(= b 0)
                (cond [(= c 0) 0]
                      [else 1])]
               [else
                (cond [(= c 0) 1]
                      [else 1])])]
        [else
         (cond [(= b 0)
                (cond [(= c 0) 1]
                      [else 0])]
               [else
                (cond [(= c 0) 0]
                      [else 0])])]))

(define (update-grid-helper row)
  (cond [(= (length row) 2) empty]
        [else (cons (rule30 (first row) (second row) (third row))
                    (update-grid-helper (rest row)))]))

(send frame show #t)