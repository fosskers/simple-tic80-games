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
(local block-sprite 258)

;; The current state of the game.
(var state {:t 0
            :ball {:x (- 120 3) :y 1}
            :rows []
            :spawn-rate 60})

(fn spawn-row []
  "Generate a new row "
  (let [row []]
    (for [i 1 max-blocks]
      (table.insert row true))
    {:y max-height :blocks row}))

(fn maybe-spawn-row [t spawn-rate rows]
  "Spawn a new row if we're on the correct tick."
  (when (= 0 (% t spawn-rate))
    (table.insert rows 1 (spawn-row)))
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

(fn rising [rows]
  "Raise every block by the rising rate."
  (each [_ row (ipairs rows)]
    (tset row :y (- row.y gravity-rate)))
  rows)

(fn gravity [ball]
  "Drop the ball."
  (tset ball :y (+ gravity-rate ball.y))
  ball)

(fn move [ball]
  "Move the ball if a button is pressed."
  (when (and (btn 2) (> (+ 1 ball.x) 0))
    (tset ball :x (- ball.x ball-rate)))
  (when (and (btn 3) (< (+ 7 ball.x) max-width))
    (tset ball :x (+ ball.x ball-rate)))
  ball)

(fn game-over? [ball]
  "Has the ball contacted the top of the screen?"
  (= 0 ball.y))

(fn _G.TIC []
  (let [ball (->> state.ball gravity move)
        rows (->> state.rows (maybe-spawn-row state.t state.spawn-rate) rising)]
    (tset state :ball ball)
    (tset state :rows rows)
    (draw ball rows)
    (print state.t))
  (tset state :t (+ 1 state.t)))

;; <SPRITES>
;; 001:000000000000000000caaa000c09a0900a900a900aa009800a0a908000998800
;; 002:c555555655555556555555565555555655555557555555675555566766667777
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

