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

(fn draw []
  (cls 2)
  ;; Draw the food first, so that the snake's head will overwrite it when
  ;; they're on the same square. This makes the "eating" look a bit better.
  (rect (* 8 food.x)
        (* 8 food.y)
        8 8 6)
  (each [_ point (ipairs snake)]
    (rect (* 8 point.x)
          (* 8 point.y)
          8 8 15)))

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

(fn _G.TIC []
  (set t (+ t 1))
  (local head (. snake (length snake)))
  (local neck (. snake (- (length snake) 1)))
  (local tail (. snake 1))
  (when (update?)
    ;; Move the snake.
    (table.insert snake {:x (% (+ head.x dir.x) 30)
                         :y (% (+ head.y dir.y) 17)})
    (if (not (got-food? head))
        (table.remove snake 1)
        (set-food)))
  ;; Change directions. Can happen any time, even if the snake isn't growing in
  ;; that frame.
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
;; 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
;; 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
;; 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
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

