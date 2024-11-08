#lang racket/gui
(require 2htdp/image)
(require 2htdp/universe)

(define max-size 1000)
(define max-grid-length 1)
(define max-grid-height 1)
;; grid is a matrix repersenting each cell
(define grid (list (list 1)))
(define rule-key (list 0 1 1 1 1 0 0 0))

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

;; seperate frame to update the rule
(define rule-frame (new frame%
                        [label "rules"]
                        [width 900]
                        [height 500]))
;; organise buttons horizontally
(define rule-panel (new horizontal-panel%
                        [parent rule-frame]
                        [spacing 10]
                        [horiz-margin 10]
                        [stretchable-height #t]
                        [alignment '(center top)]))
;; view changes to the rule
(define rule-canvas (new canvas%
                         [parent rule-frame]
                         [paint-callback (lambda (canvas dc)
                                           (draw-scene/rule dc))]))
;; buttons to change the rules
(define (update-rule-key r n)
  (cond [(= 1 n) (cond [(= 0 (first r)) (cons 1 (rest r))]
                       [else (cons 0 (rest r))])]
        [else (cons (first r) (update-rule-key (rest r) (- n 1)))]))
(define rule-button-1 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 1))
                                       (send rule-canvas on-paint)))))
(define rule-button-2 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 2))
                                       (send rule-canvas on-paint)))))
(define rule-button-3 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 3))
                                       (send rule-canvas on-paint)))))
(define rule-button-4 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 4))
                                       (send rule-canvas on-paint)))))
(define rule-button-5 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 5))
                                       (send rule-canvas on-paint)))))
(define rule-button-6 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 6))
                                       (send rule-canvas on-paint)))))
(define rule-button-7 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 7))
                                       (send rule-canvas on-paint)))))
(define rule-button-8 (new button%
                           [parent rule-panel]
                           [label "flip"]
                           (callback (lambda (button event)
                                       (set! rule-key (update-rule-key rule-key 8))
                                       (send rule-canvas on-paint)))))

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

(define (draw-scene/rule dc)
  (local [(define w (send canvas get-width))
          (define h (send canvas get-height))
          (define side-length 30)
          (define h-spacing (/ w 9))
          (define h-offset (/ h-spacing 2))
          (define v-offset side-length)
          (define (draw-rule-0)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (- h-offset side-length) v-offset side-length side-length)
            (send dc draw-rectangle h-offset v-offset side-length side-length)
            (send dc draw-rectangle (+ h-offset side-length) v-offset side-length side-length)
            (cond [(= 0 (first rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle h-offset (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-1)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (- (+ h-offset h-spacing) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ h-offset h-spacing) v-offset side-length side-length)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (+ (+ h-offset h-spacing) side-length) v-offset side-length side-length)
            (cond [(= 0 (second rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset h-spacing) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-2)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (- (+ h-offset (* 2 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ (+ h-offset (* 2 h-spacing)) side-length) v-offset side-length side-length)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (+ h-offset (* 2 h-spacing)) v-offset side-length side-length)
            (cond [(= 0 (third rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 2 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-3)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (- (+ h-offset (* 3 h-spacing)) side-length) v-offset side-length side-length)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (+ (+ h-offset (* 3 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ h-offset (* 3 h-spacing)) v-offset side-length side-length)
            (cond [(= 0 (fourth rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 3 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-4)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (- (+ h-offset (* 4 h-spacing)) side-length) v-offset side-length side-length)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (+ h-offset (* 4 h-spacing)) v-offset side-length side-length)
            (send dc draw-rectangle (+ (+ h-offset (* 4 h-spacing)) side-length) v-offset side-length side-length)
            (cond [(= 0 (fifth rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 4 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-5)
            (send dc set-brush "grey" 'cross-hatch)
            (send dc draw-rectangle (+ h-offset (* 5 h-spacing)) v-offset side-length side-length)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (- (+ h-offset (* 5 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ (+ h-offset (* 5 h-spacing)) side-length) v-offset side-length side-length)
            (cond [(= 0 (sixth rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 5 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-6)
            (send dc set-brush "grey" 'cross-hatch)

            (send dc draw-rectangle (+ (+ h-offset (* 6 h-spacing)) side-length) v-offset side-length side-length)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (- (+ h-offset (* 6 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ h-offset (* 6 h-spacing)) v-offset side-length side-length)
            (cond [(= 0 (seventh rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 6 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          (define (draw-rule-7)
            (send dc set-brush "black" 'solid)
            (send dc draw-rectangle (- (+ h-offset (* 7 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ (+ h-offset (* 7 h-spacing)) side-length) v-offset side-length side-length)
            (send dc draw-rectangle (+ h-offset (* 7 h-spacing)) v-offset side-length side-length)
            (cond [(= 0 (eighth rule-key)) (send dc set-brush "grey" 'cross-hatch)]
                  [else (send dc set-brush "black" 'solid)])
            (send dc draw-rectangle (+ h-offset (* 7 h-spacing)) (+ v-offset side-length) side-length side-length)
            )
          ]
    (send dc clear)
    (draw-rule-0)
    (draw-rule-1)
    (draw-rule-2)
    (draw-rule-3)
    (draw-rule-4)
    (draw-rule-5)
    (draw-rule-6)
    (draw-rule-7)
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
  (cond [(empty? (rest g)) (list (add-0 (first g)) (update-grid-helper (append (list 0 0) (last g) (list 0 0)) rule-template))]
        [else (cons (add-0 (first g)) (update-grid (rest g)))]))

(define (rule-template a b c)
  (cond [(= a 0)
         (cond [(= b 0)
                (cond [(= c 0) (first rule-key)]
                      [else (second rule-key)])]
               [else
                (cond [(= c 0) (third rule-key)]
                      [else (fourth rule-key)])])]
        [else
         (cond [(= b 0)
                (cond [(= c 0) (fifth rule-key)]
                      [else (sixth rule-key)])]
               [else
                (cond [(= c 0) (seventh rule-key)]
                      [else (eighth rule-key)])])]))

(define (update-grid-helper row rule)
  (cond [(= (length row) 2) empty]
        [else (cons (rule (first row) (second row) (third row))
                    (update-grid-helper (rest row) rule))]))

(send rule-panel min-height 10)
(send frame show #t)
(send rule-frame show #t)
