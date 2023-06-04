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
(local ball-rate 4)
;; The downward pull of gravity.
(local gravity-rate 1)
;; Increase the spawn rate after this many frames.
(local speed-up-frames 150)
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
;; The neutral bounding polygon of the ball.
(local ball-neutral-bbox
       [{:x 2 :y 1} {:x 3 :y 1} {:x 4 :y 1} {:x 5 :y 1}
        {:x 1 :y 2} {:x 6 :y 2}
        {:x 1 :y 3} {:x 6 :y 3}
        {:x 1 :y 4} {:x 6 :y 4}
        {:x 1 :y 5} {:x 6 :y 5}
        {:x 2 :y 6} {:x 3 :y 6} {:x 4 :y 6} {:x 5 :y 6}])
(local block-neutral-bbox
       [{:x 0 :y 0}
        {:x 1 :y 0}
        {:x 2 :y 0}
        {:x 3 :y 0}
        {:x 4 :y 0}
        {:x 5 :y 0}
        {:x 6 :y 0}
        {:x 7 :y 0}
        ;; --- ;;
        {:x 0 :y 1} {:x 7 :y 1}
        {:x 0 :y 2} {:x 7 :y 2}
        {:x 0 :y 3} {:x 7 :y 3}
        {:x 0 :y 4} {:x 7 :y 4}
        {:x 0 :y 5} {:x 7 :y 5}
        {:x 0 :y 6} {:x 7 :y 6}
        ;; --- ;;
        {:x 0 :y 7}
        {:x 1 :y 7}
        {:x 2 :y 7}
        {:x 3 :y 7}
        {:x 4 :y 7}
        {:x 5 :y 7}
        {:x 6 :y 7}
        {:x 7 :y 7}])
;; A barrier at the bottom of the screen.
(local floor-poly
       (let [acc []]
         (for [i 0 max-width]
           (table.insert acc {:x i :y max-height}))
         acc))
(local left-wall
       (let [acc []]
         (for [i 0 max-height]
           (table.insert acc {:x -1 :y i})
           (table.insert acc {:x -8 :y i}))
         acc))
(local right-wall
       (let [acc []]
         (for [i 0 max-height]
           (table.insert acc {:x max-width :y i})
           (table.insert acc {:x (+ 8 max-width) :y i}))
         acc))

;; The current state of the game.
(var state {:t 0
            :ball {:x (- 120 3) :y 1}
            :past-balls []
            :ball-bounds []
            :rows []
            :spawn-rate-max 30
            :spawn-rate-curr 0
            :paused false})

(fn dbg-draw-bbox [bbox]
  "Draw a given bounding polygon."
  (let [red 2]
    (each [_ {:x x :y y} (ipairs bbox)]
      (pix x y red))))

(fn block-bounds [screen-x screen-y]
  "Yield a bounding polygon of a block, given the top-left XY coordinates of
 its sprite."
 (icollect [_ {:x x :y y} (ipairs block-neutral-bbox)]
   {:x (+ x screen-x) :y (+ y screen-y)}))

(fn ball-bounds [screen-x screen-y]
  "Yield a bounding polygon of the ball, given the top-left XY coordinates of
 its sprite."
 (icollect [_ {:x x :y y} (ipairs ball-neutral-bbox)]
   {:x (+ x screen-x) :y (+ y screen-y)}))

(fn all? [items]
  "Are all the items in a collection true?"
  (accumulate [all true _ item (ipairs items) &until (not all)]
    item))

(fn spawn-row []
  "Generate a new row."
  (let [row []]
    (for [i 1 max-blocks]
      (let [spawn? (~= 1 (math.random 1 (/ max-blocks 5)))]
        (table.insert row spawn?)))
    (when (all? row)
      (table.insert row 1 false))
    {:y max-height :blocks row}))

(fn maybe-spawn-row [spawn-rate rows]
  "Spawn a new row if we're on the correct tick."
  (when (= 0 spawn-rate)
    (table.insert rows 1 (spawn-row)))
  rows)

(fn oldest-row-off-screen? [rows]
  "Has the oldest block row moved off the top of the screen?"
  (and (> (length rows) 0)
       (let [y (. (. rows (length rows)) :y)]
         (<= (+ 7 y) 0))))

(fn cull-last-row [rows]
  "Potentially delete the oldest row, if it has moved past the top of the screen."
  (when (oldest-row-off-screen? rows)
    (table.remove rows (length rows)))
  rows)

(fn draw-row [{:y y :blocks row}]
  "Draw an entire row of obstacle blocks."
  (each [i block (ipairs row)]
    (when block
      (spr block-sprite (* (- i 1) 8) y transparency))))

(fn draw-ball [ball]
  "Draw the ball."
  (spr ball-sprite ball.x ball.y transparency))

(fn draw [ball past-balls rows]
  "Draw all sprites."
  (cls background)
  (each [_ past (ipairs past-balls)]
    (draw-ball past))
  (draw-ball ball)
  (each [_ row (ipairs rows)]
    (draw-row row)))

(fn raise-rows [rows]
  "Raise every block by the rising rate."
  (each [_ row (ipairs rows)]
    (tset row :y (- row.y gravity-rate)))
  rows)

(fn at-bottom? [ball]
  "Is the ball at the bottom of the screen?"
  (>= (+ 8 ball.y) max-height))

;; (fn overlap? [a b]
;;   "Are two bounding polygons overlapping somewhere?"
;;   (let [{: x-min : x-max : y-min : y-max} (bounding-rectangle b)]
;;     (accumulate [overlapping? false _ {:x x :y y} (ipairs a) &until overlapping?]
;;       (and (<= x-min x x-max)
;;            (<= y-min y y-max)))))

(fn movement-vector [left? right?]
  "A vector to move the ball in, depending on the user input."
  (let [y gravity-rate
        x (if (and left? right?) 0
              left? (* -1 ball-rate)
              right? ball-rate
              0)]
    {:x x :y y}))

(fn game-over? [ball]
  "Has the ball contacted the top of the screen?"
  (= 0 (+ 2 ball.y)))

(fn dbg-mvec [mvec]
  "Display the current movement vector of the ball."
  (print (string.format "{:x %d :y %d}" mvec.x mvec.y) 0 8))

;; (fn dbg-detection [rows ball]
;;   "Display bounding polygons of blocks that the ball is contacting during this frame."
;;   (var collisions 0)
;;   (let [ba-bounds (ball-bounds ball.x ball.y)]
;;     (each [_ {:y y :blocks row} (ipairs rows)]
;;       (each [i block? (ipairs row)]
;;         (when block?
;;           (let [bl-bounds (block-bounds (* (- i 1) 8) y)]
;;             (when (overlap? ba-bounds bl-bounds)
;;               (set collisions (+ 1 collisions))
;;               (dbg-draw-bbox bl-bounds)))))))
;;   (print (string.format "Collisions: %d" collisions)))

;; --- COLLISION --- ;;

(fn translate [poly {:x dx :y dy}]
  "Given some baseline bounding polygon and coordinates to shift it by, shift it."
  (icollect [_ {: x : y} (ipairs poly)]
    {:x (+ x dx)
     :y (+ y dy)}))

;; (translate ball-neutral-bbox {:x 0 :y 1})

(fn bounding-rectangle [poly]
  "Yield the min/max X and Y values of some polygon."
  (let [init {:x-min math.maxinteger
              :x-max math.mininteger
              :y-min math.maxinteger
              :y-max math.mininteger}]
    (accumulate [{: x-min : x-max : y-min : y-max} init _ {: x : y} (ipairs poly)]
      {:x-min (math.min x-min x)
       :x-max (math.max x-max x)
       :y-min (math.min y-min y)
       :y-max (math.max y-max y)})))

;; (bounding-rectangle block-neutral-bbox)

(fn collisions [a b]
  "All points of a polygon `a` that collide with the bounding rectangle of a
  polygon `b`."
  (let [{: x-min : x-max : y-min : y-max} (bounding-rectangle b)]
    (accumulate [acc [] _ point (ipairs a)]
      (if (and (<= x-min point.x x-max)
               (<= y-min point.y y-max))
          (do (table.insert acc point) acc)
          acc))))

;; (let [ball ball-neutral-bbox
;;       block (translate block-neutral-bbox {:x 3 :y 5})]
;;   (collisions ball block))

(fn vertical-reflect [colliding b]
  "Yield a vertical reflection vector, given some collision points and a polygon
  `b`. It's assumed that the original colliding object is coming straight down."
  (let [colliding (collisions colliding b)]
    (if (= 0 (length colliding))
      {:x 0 :y 0}
      (let [a-lowest  (accumulate [seen math.mininteger _ {: y} (ipairs colliding)] (math.max seen y))
            b-highest (accumulate [seen math.maxinteger _ {: y} (ipairs b)] (math.min seen y))]
        {:y (* -1 (+ 1 (math.abs (- a-lowest b-highest))))
         :x 0}))))

(fn left-reflect [colliding b]
  (let [colliding (collisions colliding b)]
    (if (= 0 (length colliding))
      {:x 0 :y 0}
      (let [a-leftest  (accumulate [seen math.maxinteger _ {: x} (ipairs colliding)] (math.min seen x))
            b-rightest (accumulate [seen math.mininteger _ {: x} (ipairs b)] (math.max seen x))]
        {:x (+ 1 (math.abs (- a-leftest b-rightest)))
         :y 0}))))

(fn right-reflect [colliding b]
  (let [colliding (collisions colliding b)]
    (if (= 0 (length colliding))
      {:x 0 :y 0}
      (let [a-rightest (accumulate [seen math.mininteger _ {: x} (ipairs colliding)] (math.max seen x))
            b-leftest  (accumulate [seen math.maxinteger _ {: x} (ipairs b)] (math.min seen x))]
        {:x (* -1 (+ 1 (math.abs (- a-rightest b-leftest))))
         :y 0}))))

(fn reflect [ball block mvec]
  "Given the current position of a ball and block, and the desired movement
  vector of the ball, yield a (potentially) modified vector that takes collision
  into account."
  (let [desired   (translate ball mvec)
        colliding (collisions desired block)]
    (if (= 0 (length colliding))
      mvec
      (let [x-only (translate ball {:x mvec.x :y 0})
            x-coll (collisions x-only block)
            x-refl (if (< mvec.x 0)
                     (left-reflect x-coll block)
                     (right-reflect x-coll block))
            x-ball (translate ball {:x (+ mvec.x x-refl.x) :y x-refl.y})
            moved  (translate x-ball {:x 0 :y mvec.y})
            y-coll (collisions moved block)
            y-refl (vertical-reflect y-coll block)]
        {:x (+ mvec.x x-refl.x x-refl.y)
         :y (+ mvec.y y-refl.x y-refl.y)}))))

;; (let [mvec  {:x 0 :y 1}
;;       ball  (translate ball-neutral-bbox {:x 0 :y 0})
;;       block (translate block-neutral-bbox {:x 0 :y 7})]
;;   (reflect ball block mvec))

;; --- MOVEMENT --- ;;

(fn nearby-polys [ball rows mvec]
  "Any polygons within the given `rows` which are within range of the ball's
desired movement."
  (let [desired (translate ball mvec)
        min-y   (accumulate [seen math.maxinteger _ {: y} (ipairs desired)] (math.min seen y))
        max-y   (accumulate [seen math.mininteger _ {: y} (ipairs desired)] (math.max seen y))
        row?    (accumulate [in-range nil _ {:y y :blocks row} (ipairs rows) &until in-range]
                  (when (or (<= y min-y (+ y 7))
                            (<= y max-y (+ y 7)))
                    [y row]))
        defaults [floor-poly left-wall right-wall]]
    (if (not row?) defaults
        (let [[y row] row?]
          (accumulate [acc defaults i block? (ipairs row)]
            (if block?
                (let [block (translate block-neutral-bbox {:x (* 8 (- i 1)) :y y})]
                  (if (> (length (collisions desired block)) 0)
                      (do (table.insert acc 1 block) acc)
                      acc))
                acc))))))

(fn adjust-for-collision [ball rows mvec]
  "Attempt to move the ball. It does this by colliding with each nearby poly to
find the resulting actual movement vector, gradually diminishing it until all
collisions have been made."
  (let [ball-poly (translate ball-neutral-bbox ball)  ;; Correct?
        nearby    (nearby-polys ball-poly rows mvec)
        final     (accumulate [vec mvec _ poly (ipairs nearby)]
                    (reflect ball-poly poly vec))]
    [final nearby]))

(fn quick-colliding? [ball rows]
  "Is the ball colliding downwards with any blocks?"
  (let [bottom (+ 6 ball.y)
        left   (+ 1 ball.x)
        right  (+ 6 ball.x)]
    (accumulate [colliding? false _ {:y y :blocks row} (ipairs rows) &until colliding?]
      (accumulate [colliding? false i block? (ipairs row) &until colliding?]
        (and block?
             (= bottom y)
             (let [foo (* 8 (- i 1))]
               (or (<= foo left  (+ 7 foo))
                   (<= foo right (* 7 foo)))))))))

;; --- GAME LOOP --- ;;

(fn _G.TIC []
  (when (or (not state.paused)
            (btn 1))
    (table.insert state.past-balls 1 state.ball)
    (when (> (length state.past-balls) 5)
      (table.remove state.past-balls (length state.past-balls)))
    ;; --- Physics --- ;;
    (let [left?  (btn 2)
          right? (btn 3)
          desire (movement-vector left? right?)
          [mvec nearby] (adjust-for-collision state.ball state.rows desire)
          ball   {:x (+ state.ball.x mvec.x)
                  :y (+ state.ball.y mvec.y)}
          rows   (->> state.rows
                      (maybe-spawn-row state.spawn-rate-curr)
                      cull-last-row
                      raise-rows)
          coll?  (quick-colliding? ball rows)
          ball   (if coll? {:x ball.x :y (- ball.y 1)} ball)]
      (when coll?
        (let [shadows (icollect [_ {: x : y} (ipairs state.past-balls)]
                        {:x x :y (- y 1)})]
          (tset state :past-balls shadows)))
      (tset state :ball ball)
      (tset state :rows rows)
      ;; (tset state :ball-bounds (ball-bounds ball.x ball.y))
      ;; --- Rendering --- ;;
      (draw ball state.past-balls rows)
      ;; (print (string.format "D: (%d, %d)" desire.x desire.y) 0 8)
      ;; (print (string.format "A: (%d, %d)" mvec.x mvec.y) 0 16)
      ;; (print (string.format "Coll? %s" coll?) 0 24)
      ;; (each [_ poly (ipairs nearby)]
      ;;   (each [_ {: x : y} (ipairs poly)]
      ;;     (pix x y 2)))
      ;; (dbg-draw-bbox state.ball-bounds)
      ;; (dbg-detection rows ball)
      ;; (dbg-mvec mvec)
      ;; (print (string.format "Spawn Rate: %d" state.spawn-rate-curr))
      ;; --- Game over check --- ;;
      (when (game-over? ball)
        (trace (string.format "Game over! Score: %d" state.t))
        (exit)))
    ;; Adjust the block spawn rate if necessary.
    (if (= 0 state.spawn-rate-curr)
        (tset state :spawn-rate-curr state.spawn-rate-max)
        (tset state :spawn-rate-curr (- state.spawn-rate-curr 1)))
    ;; The slow, steady march of time.
    (tset state :t (+ 1 state.t))
    (when (= 0 (% state.t speed-up-frames))
      (tset state :spawn-rate-max (- state.spawn-rate-max 1)))
    (print (string.format "Score: %d" state.t)))
    ;; (print (string.format "Spawn Rate: %d" state.spawn-rate-max) 0 8))
  ;; Have they paused or unpaused the game?
  (when (key 16)
    (tset state :paused (not state.paused))))

;; <TILES>
;; 001:c555555655555556555555565555555655555557555555675555566766667777
;; </TILES>

;; <SPRITES>
;; 001:0000000000caaa000c09a0900a900a900aa009800a0a90800099880000000000
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

