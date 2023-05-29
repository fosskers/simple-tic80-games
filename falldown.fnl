;; title:   Falldown
;; author:  Colin Woodbury <colin@fosskers.ca>
;; desc:    Fall as far as you can! The classic TI-83 game reborn.
;; site:    https://git.sr.ht/~fosskers/simple-tic80-games
;; license: GPLv3
;; version: 0.1
;; script:  fennel
;; strict:  true
;; input:   keyboard

;; The index of the colour to be treated as transparent in sprites.
(local transparency 0)
;; The index of the background colour.
(local background 13)
;; The left-right movement speed of the ball.
(local ball-rate 2)
;; The downward pull of gravity.
(local gravity-rate 1)
;; The maximum width of the screen.
(local max-width 240)
;; The maximum height of the screen.
(local max-height 136)
;; The number of blocks that can fit in a single row.
(local max-blocks (/ max-width 8))
;; The index of the ball sprite.
(local ball-sprite 257)
;; The index of the block sprite.
(local block-sprite 1)

;; The current state of the game.
(var state {:t 0
            :ball {:x (- 120 3) :y 1}
            :rows []
            :spawn-rate 60})

(fn spawn-row []
  "Generate a new row."
  (let [row []]
    (for [i 1 max-blocks]
      (let [spawn? (~= 1 (math.random 1 (/ max-blocks 5)))]
        (table.insert row spawn?)))
    {:y max-height :blocks row}))

(fn maybe-spawn-row [t spawn-rate rows]
  "Spawn a new row if we're on the correct tick."
  (when (= 0 (% t spawn-rate))
    (table.insert rows 1 (spawn-row)))
  rows)

(fn cull-last-row [rows]
  "Potentially delete the oldest row, if it has moved past the top of the screen."
  (let [y (. (. rows (length rows)) :y)]
    (when (<= (+ 7 y) 0)
      (table.remove rows (length rows))))
  rows)

(fn draw-row [{:y y :blocks row}]
  "Draw an entire row of obstacle blocks."
  (each [i block (ipairs row)]
    (when block
      (spr block-sprite (* (- i 1) 8) y transparency))))

(fn draw-ball [ball]
  "Draw the ball."
  (spr ball-sprite ball.x ball.y transparency))

(fn draw [ball rows]
  "Draw all sprites."
  (cls background)
  (draw-ball ball)
  (each [_ row (ipairs rows)]
    (draw-row row)))

(fn raise-rows [rows]
  "Raise every block by the rising rate."
  (each [_ row (ipairs rows)]
    (tset row :y (- row.y gravity-rate)))
  rows)

(fn horizontal-contact? [row ball]
  "Is the ball horizontally contacting the outside of a block?"
  (accumulate [contact? false i block? (ipairs row) &until contact?]
    (and block?
         (let [block-l (* (- i 1) 8)
               block-r (* (+ 7 block-l))]
           (or (= block-r ball.x)
               (= block-l (+ 7 ball.x)))))))

(fn horizontal-overlap? [row ball]
  "Is the ball within the x-range of any present blocks?"
  (accumulate [overlap? false i block? (ipairs row) &until overlap?]
    (and block?
         (let [block-l (* (- i 1) 8)
               block-r (+ 7 block-l)]
           ;; It's only necessary to check the two corner points of the
           ;; ball, not its entire bounding box.
           (or (<= block-l (+ 1 ball.x) block-r)
               (<= block-l (+ 6 ball.x) block-r))))))

(fn colliding-down? [rows ball]
  "Is the ball colliding in the downward direction with some blocks?"
  (accumulate [colliding? false _ {:y y :blocks row} (ipairs rows) &until colliding?]
    (and (= y (+ ball.y 8))
         (horizontal-overlap? row ball))))

(fn overlapping-down? [rows ball]
  "Is the ball overlapping in the downward direction with some blocks?"
  (accumulate [colliding? false _ {:y y :blocks row} (ipairs rows) &until colliding?]
    (and (< y (+ ball.y 8) (+ 7 y))
         (horizontal-overlap? row ball))))

(fn gravity [ball]
  "Drop the ball."
  (tset ball :y (+ ball.y gravity-rate))
  ball)

(fn raise-ball [ball]
  "Collision has occurred and the ball must be raised."
  (tset ball :y (- ball.y gravity-rate))
  ball)

(fn at-bottom? [ball]
  "Is the ball at the bottom of the screen?"
  (>= (+ 8 ball.y) max-height))

(fn maybe-gravity [rows ball]
  "Apply gravity if there's no downward collision."
  (if (colliding-down? rows ball) ball
      (overlapping-down? rows ball) (raise-ball ball)
      (at-bottom? ball) ball
      (gravity ball)))

(fn move [ball]
  "Move the ball if a button is pressed."
  (when (and (btn 2) (> (+ 1 ball.x) 0))
    (tset ball :x (- ball.x ball-rate)))
  (when (and (btn 3) (< (+ 7 ball.x) max-width))
    (tset ball :x (+ ball.x ball-rate)))
  ball)

(fn horizontal-contacts? [rows ball]
  "If the ball contacting any blocks horizontally?"
  (accumulate [colliding? false _ {:y y :blocks row} (ipairs rows) &until colliding?]
    (and (<= y (+ 7 ball.y) (+ 7 y))
         (horizontal-contact? row ball))))

(fn maybe-move [rows ball]
  "Move the ball if it's not colliding horizontally with a block."
  (if (horizontal-contacts? rows ball) ball
      (move ball)))

(fn game-over? [ball]
  "Has the ball contacted the top of the screen?"
  (= 0 (+ 2 ball.y)))

(fn _G.TIC []
  (let [rows (->> state.rows
                  (maybe-spawn-row state.t state.spawn-rate)
                  cull-last-row
                  raise-rows)
        ball (->> state.ball (maybe-gravity rows) (maybe-move rows))]
    (tset state :ball ball)
    (tset state :rows rows)
    (draw ball rows)
    (print (string.format "Rows: %d" (length state.rows)))
    (when (game-over? ball)
      (trace (string.format "Game over! Score: %d" state.t))
      (exit)))
  (tset state :t (+ 1 state.t)))

;; <TILES>
;; 001:c555555655555556555555565555555655555557555555675555566766667777
;; </TILES>

;; <SPRITES>
;; 001:000000000000000000caaa000c09a0900a900a900aa009800a0a908000998800
;; </SPRITES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

