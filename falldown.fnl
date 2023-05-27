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

;; The current state of the game.
(var state {:t 0
            :ball {:x (- 120 3) :y 1}})

(fn draw-ball [ball]
  "Draw the ball."
  (spr 1 ball.x ball.y transparency))

(fn draw [ball]
  "Draw all sprites."
  (cls background)
  (draw-ball ball))

(fn gravity [ball]
  "Drop the ball."
  (tset ball :y (+ gravity-rate ball.y))
  ball)

(fn move [ball]
  "Move the ball if a button is pressed."
  (when (and (btn 2) (> (+ 1 ball.x) 0))
    (tset ball :x (- ball.x ball-rate)))
  (when (and (btn 3) (< (+ 7 ball.x) 240))
    (tset ball :x (+ ball.x ball-rate)))
  ball)

(fn game-over? [ball]
  "Has the ball contacted the top of the screen?"
  (= 0 ball.y))

(fn _G.TIC []
  (let [ball (-> state.ball gravity move)]
    (draw ball))
  (tset state :t (+ 1 state.t)))

;; <TILES>
;; 001:000000000000000000caaa000c09a0900a900a900aa009800a0a908000998800
;; 002:c555555655555556555555565555555655555557555555675555566766667777
;; </TILES>

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

