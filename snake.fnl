;; title:   Fennel Snake
;; author:  Colin Woodbury, <colin@fosskers.ca>
;; desc:    Snake, the classic game.
;; site:    website link
;; license: GPLv3
;; version: 0.1
;; script:  fennel
;; strict:  true

;; Directions to move the head in.
(local dirs [{:x 0  :y -1}
             {:x 0  :y 1}
             {:x -1 :y 0}
             {:x 1  :y 0}])

(var t 0)
(var score 0)
(var snake [{:x 15 :y 8}    ;; tail
            {:x 14 :y 8}    ;; neck
            {:x 13 :y 8}])  ;; head
(var food {:x 1 :y 0})
(var dir (. dirs 1))

(fn update? []
  "Only move the Snake every 6 frames."
  (= 0 (% t 10)))

(fn draw-food []
  "Draw the Fennel."
  (spr 1 (* 8 food.x) (* 8 food.y) 0))

(fn draw-snake []
  (each [_ point (ipairs snake)]
      (rect (* 8 point.x)
            (* 8 point.y)
            8 8 15)))

(fn draw []
  (cls 2)
  ;; Draw the food first, so that the snake's head will overwrite it when
  ;; they're on the same square. This makes the "eating" look a bit better.
  (draw-food)
  (draw-snake))

(fn got-food? [head]
  "Did we find the food this turn?"
  (and (= head.x food.x)
       (= head.y food.y)))

(fn any? [p coll]
  "Does any element of the collection satisfy the predicate?"
  (accumulate [found false _ x (ipairs coll) &until found]
    (p x)))

(fn set-food []
  "Move the food to a new random square."
  (set food.x (math.random 0 29))
  (set food.y (math.random 0 16))
  (when (any? (fn [piece] (and (= piece.x food.x)
                               (= piece.y food.y)))
              snake)
    (set-food)))

(fn colliding? [head body]
  "Is the head colliding with the body?"
  (accumulate [colliding false i piece (ipairs body) &until colliding]
    (and (not (= i (length body)))  ;; The head itself.
         (= head.x piece.x)
         (= head.y piece.y))))

(fn _G.TIC []
  (set t (+ t 1))
  (var head (. snake (length snake)))
  (when (update?)
    ;; Move the snake.
    (table.insert snake {:x (% (+ head.x dir.x) 30)
                         :y (% (+ head.y dir.y) 17)})
    (set head (. snake (length snake)))
    (if (colliding? head snake) (do (trace "Game over!") (exit))
        (not (got-food? head)) (table.remove snake 1)
        (set-food)))
  ;; Change directions. Can happen any time, even if the snake isn't growing in
  ;; that frame.
  (local neck (. snake (- (length snake) 1)))
  (local last-dir dir)
  (if (btn 0) (set dir (. dirs 1))
      (btn 1) (set dir (. dirs 2))
      (btn 2) (set dir (. dirs 3))
      (btn 3) (set dir (. dirs 4)))
  (when (and (= neck.x (+ head.x dir.x))
             (= neck.y (+ head.y dir.y)))
    (set dir last-dir))
  (draw))

;; <TILES>
;; 001:06006000006060600006660000cccc000cccccc0cc0cc0cccccccccc0cccccc0
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

