(def title "Vampyre")
(def debug true)

(def height (if debug 110 28))
(def width  (if debug 110 28))
(def scale  (if debug   1  3))

(def HEIGHT 100)
(def WIDTH  100)

(def sprites [
    [ "["
      1 1 1 1 1 1 0
      1 1 1 1 1 1 0
      1 0 0 0 0 1 0
      1 1 1 1 1 1 0
      1 0 0 0 0 1 0
      1 1 1 1 1 1 0
      0 0 0 0 0 0 0
    ]
    [ "|"
      1 1 1 1 1 1 0
      1 0 0 0 0 1 0
      1 0 0 0 0 1 0
      1 0 0 0 0 1 0
      1 0 0 0 0 1 0
      1 1 1 1 1 1 0
      0 0 0 0 0 0 0
    ]
    [ "&"
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
      0 0 1 1 0 0 0
      0 0 1 1 0 0 0
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
    ]
    [ "*"
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
      0 0 0 1 0 0 0
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
      0 0 0 0 0 0 0
    ]
    [ "#"
      1 1 1 1 1 1 0
      0 0 0 0 0 1 0
      1 0 1 1 1 1 0
      0 0 0 1 0 0 0
      1 1 1 1 1 1 0
      1 0 1 1 0 1 0
      0 0 0 0 0 0 0
    ]
])

(var msg "chilly...")
(var user-goto nil)
(var startx 0)
(var starty 0)
(var endx 0)
(var endy 0)

(def tile-proto @{ :type :wall
                   :mob -1
                   :walkable? (fn [tile]
                                (and
                                  (or
                                    (= (tile :type) :floor)
                                    (= (tile :type) :open-door))
                                  (= (tile :mob) -1)))
                 })
(var dungeon (array/new HEIGHT))

(var fuses @[])

(def D_N  [ -1   0 ])
(def D_S  [  1   0 ])
(def D_E  [  0   1 ])
(def D_W  [  0  -1 ])
(def D_NW [ -1  -1 ])
(def D_NE [ -1   1 ])
(def D_SW [  1  -1 ])
(def D_SE [  1   1 ])

(def directions [ D_NW D_N D_NE D_W
                  D_E D_SW D_S D_SE ])

(defn coord-valid? [c]
  (and (>= (c 0) 0) (>= (c 1) 0)
       (< (c 1) WIDTH) (< (c 0) HEIGHT)))

(defn coord-add [a b]
  (def r [(+ (a 0) (b 0)) (+ (a 1) (b 1))])
  (if (not (coord-valid? r)) nil r))

(defn manhattan-distance [a b]
  (def diff [ (math/abs (- (a 0) (b 0)))
              (math/abs (- (a 1) (b 1)))
            ])
  (+ (diff 0) (diff 1)))

(defn at-dungeon [coord]
  ((dungeon (coord 0)) (coord 1)))

(def mob-proto @{ :id -1
                  :tile "g"
                  :coord [-1 -1]
                  :max-hp 10
                  :hp 10
                  :fov-radius (- (/ (max height width) 2) 3)
                  :fov @{}
                  :move-mob (fn [mob dir]
                              (def src (mob :coord))
                              (def dst (coord-add src dir))
                              (if dst
                                (do
                                  (def tile (at-dungeon dst))
                                  (cond
                                    (= (tile :type) :closed-door)
                                      (do
                                        (set (tile :type) :open-door)
                                        (array/push fuses @[4 (fn [c]
                                                               (set (tile :type) :closed-door))
                                                           dst])
                                        true)
                                    (:walkable? (at-dungeon dst))
                                      (do
                                        (set ((at-dungeon src) :mob) -1)
                                        (set ((at-dungeon dst) :mob) (mob :id))
                                        (set (mob :coord) dst)
                                        true)
                                    false))
                                false))
                })
(var mobs @[])
(var player -1)
(var memory @{})

(defn load-sprites [data]
    (loop [sprite :in data]
        (def char (- ((string/bytes (sprite 0)) 0) 32))
        (def offset (+ 0x4040 (* char 7 7)))
        (var i 0)
        (loop [pixel :in (tuple/slice sprite 1)]
          (poke (+ offset i) pixel)
          (++ i))))

(defn shuffle [arr]
  (defn swap [arr i1 i2]
    (def tmp (arr i1))
    (set (arr i1) (arr i2))
    (set (arr i2) tmp))
  (loop [ctr :range [0 (length arr)]]
    (def i (math/floor (% (* (math/random) 100000) (length arr))))
    (swap arr i ctr)))

(defn // [arg1 & args]
  (var accm arg1)
  (each arg args
    (/= accm arg))
  (math/round accm))

(defn shadowcast [from radius]
  (def mult [ [  1  0  0 -1 -1  0  0  1 ]
              [  0  1 -1  0  0 -1  1  0 ]
              [  0  1  1  0  0 -1 -1  0 ]
              [  1  0  0  1 -1  0  0 -1 ]
            ])
  (defn cast-light [buf coord row start_p end radius coordx coordy]
    (if (< start_p end) (break))
    (var start start_p)
    (var new-start 0)
    (var j row)
    (var stepj (if (< row radius) 1 -1))
    (while (< j radius)
      (def dy (- j))
      (var dx (- (- j) 1))
      (var blocked false)
      (while (<= dx 0)
        (++ dx)
        (def curx (+ (coord 1) (* dx (coordx 1)) (* dy (coordx 0))))
        (def cury (+ (coord 0) (* dx (coordy 1)) (* dy (coordy 0))))
        (def cur [cury curx])
        (if (coord-valid? cur)
          (do
            (def l-slope (/ (- dx 0.5) (+ dy 0.5)))
            (def r-slope (/ (+ dx 0.5) (- dy 0.5)))
            (if (>= start r-slope)
              (do
                (if (> end l-slope) (break))
                (if (<= (+ (* dx dx) (* dy dy)) (* radius radius))
                  (set (buf cur) true))
                (if blocked
                  (if (= ((at-dungeon cur) :type) :wall)
                    (set new-start r-slope)
                    (do (set blocked false)
                        (set start new-start)))
                  (if (and (= ((at-dungeon cur) :type) :wall)
                           (< j radius))
                    (do
                      (set blocked true)
                      (cast-light buf coord (+ j 1) start l-slope radius coordx coordy)
                      (set new-start r-slope)))))))))
      (if blocked (break))
      (+= j stepj)))

  (var res @{})
  (set (res from) true)
  (loop [octant :range [0 8]]
    (cast-light res from 1 1 0 radius
                [((mult 0) octant) ((mult 1) octant)]
                [((mult 2) octant) ((mult 3) octant)]))
  res)

(defn bresenham-circle [center radius]
  (defn add-coord [buf x y]
    (if (coord-valid? [y x]) (array/push buf [y x])))

  (var res @[])

  (def circum (math/ceil (* (* math/pi 2) radius)))
  (def y (center 0))
  (def x (center 1))

  (var f (- 1 radius))
  (var ddf-x 0)
  (var ddf-y (* -2 radius))
  (var dx 0)
  (var dy radius)

  (add-coord res x (+ y radius))
  (add-coord res x (- y radius))
  (add-coord res (+ x radius) y)
  (add-coord res (- x radius) y)

  (while (< dx dy)
    (if (>= f 0)
      (do
        (-- dy)
        (+= ddf-y 2)
        (+= f ddf-y)))

    (+= dx 1)
    (+= ddf-x 2)
    (+= f (+ ddf-x 1))

    (add-coord res (+ x dx) (+ y dy))
    (add-coord res (- x dx) (+ y dy))
    (add-coord res (+ x dx) (- y dy))
    (add-coord res (- x dx) (- y dy))
    (add-coord res (+ x dy) (+ y dx))
    (add-coord res (- x dy) (+ y dx))
    (add-coord res (+ x dy) (- y dx))
    (add-coord res (- x dy) (- y dx)))

  res)

# If &restrict is truthy, then the algorithm will consider tiles
# that the player cannot see or remember as blocking
(defn astar [start goal hfn &restrict]
  (def NODE_CLOSED 0)
  (def NODE_OPEN 1)

  (defn node-f [self]
    (+ (self :g) (self :h)))

  (defn node-valid? [self restrict]
    (and (or
           (:walkable? (at-dungeon self))
           (= ((at-dungeon self) :type) :closed-door))
         (or (not restrict)
             (and restrict (or (((mobs player) :fov) self) (memory self))))))

  (cond
    (not (node-valid? goal &restrict)) (break nil)
    (= start goal) (break @[goal]))

  (var nodes @{})
  (var open-list @[])
  (set (nodes start) @{ :coord start
                        :parent nil
                        :g 0
                        :h (hfn start goal)
                        :state NODE_OPEN
                      })
  (array/push open-list start)

  (var res nil)

  (while (> (length open-list) 0)
    (var cur nil)
    (var best-fscore 9999)
    (var cur-ind 0)

    (var i 0)
    (loop [coord :in open-list]
      (def fscore (node-f (nodes coord)))
      (if (< fscore best-fscore)
        (do
          (set cur (nodes coord))
          (set best-fscore fscore)
          (set cur-ind i)))
      (++ i))

    (array/remove open-list cur-ind)

    (if (= (cur :coord) goal)
      (do
        (set res @[])
        (while (cur :parent)
          (array/push res (cur :coord))
          (def parent (cur :parent))
          (set cur (nodes (cur :parent))))
        (break)))

    (loop [direction :in directions]
      (def neighbor (coord-add (cur :coord) direction))
      (var neighbor-g (+ (cur :g) 1))
      (def existing-node (nodes neighbor))

      (if (and (node-valid? neighbor &restrict)
               (or (not existing-node) (= (existing-node :state) NODE_OPEN)))
        (do
          (if (and existing-node
                   (= (existing-node :state) NODE_OPEN)
                   (> neighbor-g (existing-node :g)))
            (do
              (var ind 0)
              (loop [coord :in open-list]
                (if (= coord neighbor) (break))
                (++ ind))
              (array/remove open-list ind)))

          (set (nodes neighbor) @{ :coord neighbor
                                   :parent (cur :coord)
                                   :g neighbor-g
                                   :h (hfn neighbor goal)
                                   :state NODE_OPEN
                                   :ol-ind (length open-list)
                                 })
          (array/push open-list neighbor))))
      (set (cur :state) NODE_CLOSED))

  res)

(defn calc-display-offsets []
  (def player-coord ((mobs player) :coord))
  (def mapwidth (- width 1))
  (def mapheight (- height 2))

  (set startx (- (player-coord 1) (//  mapwidth 2)))
  (set starty (- (player-coord 0) (// mapheight 2)))
  (set endx (+ (player-coord 1) (//  mapwidth 2)))
  (set endy (+ (player-coord 0) (// mapheight 2))))

(defn draw []
  (fill 0 0 width height " ")
  (calc-display-offsets)

  (var y 0)
  (loop [my :range [starty endy]]
    (var x 0)
    (loop [mx :range [startx endx]]
      (if (coord-valid? [my mx])
        (do
          (def tile (at-dungeon [my mx]))

          (color
            (if (((mobs player) :fov) [my mx])
              (match (tile :type)
                (@ :wall) 0x0D
                (@ :closed-door) 0x0C
                (@ :open-door) 0x0C
                (@ :floor) 0x0E)
              (if (memory [my mx])
                0x0F
                (if debug 0x0F 0x00))))

          (var s
            (match (tile :type)
              (@ :closed-door) "["
              (@ :open-door) "|"
              (@ :wall) "#"
              (@ :floor) "*"))

          (if (not= (tile :mob) -1)
            (do
              (set s ((mobs (tile :mob)) :tile))
              (color 0x01)))

          (c7put x y s)))
      (++ x))
    (++ y))
 
  (color 0x0E)
  (c7put 0 (- height 1) msg)
  
  (if user-goto
    (do
      (color 12)
      (loop [coord :in user-goto]
        (def dx (- (coord 1) startx))
        (def dy (- (coord 0) starty))
        (if (= ((at-dungeon coord) :type) :floor)
          (c7put dx dy "&")))
      (set user-goto nil))))

(defn mapgen []
  (defn fill-random-circles [num max-radius tiletype]
    (loop [_ :range [0 num]]
      (def rx (math/round (% (* (math/random) 1000) WIDTH)))
      (def ry (math/round (% (* (math/random) 1000) HEIGHT)))
      (def radius (+ 2 (math/round (% (* (math/random) 1000) max-radius))))

      (loop [r :range [1 max-radius]]
        (def circle (bresenham-circle [ry rx] r))
        (loop [coord :in circle]
          (set ((at-dungeon coord) :type) tiletype)))))

  (defn dig-maze [coord]
    (def neighbors @[ [[ -1  0 ] [ -2  0 ]]
                      [[  1  0 ] [  2  0 ]]
                      [[  0  1 ] [  0  2 ]]
                      [[  0 -1 ] [  0 -2 ]]
                    ])
    (shuffle neighbors)
    (loop [neighbor :in neighbors]
      (def icoord (coord-add coord (neighbor 0)))
      (def ncoord (coord-add coord (neighbor 1)))
      (if (and icoord ncoord
               (not= ((at-dungeon ncoord) :type) :floor))
        (do
          (set ((at-dungeon icoord) :type) :floor)
          (set ((at-dungeon ncoord) :type) :floor)
          (dig-maze ncoord)))))

  # Disabled for now.
  #(dig-maze [0 0])
  #(fill-random-circles 50 9 :wall)

  (var cur [(/ WIDTH 2) (/ HEIGHT 2)])
  (var run 1)
  (var last-dir D_N)

  (loop [_ :range [0 10000]]
    (set ((at-dungeon cur) :type) :floor)
    (-- run)
    (if (<= run 0)
      (do
        (def rnd (% (* (math/random) 100) 6))
        (def new-dir (cond
                       (< rnd 1) D_N
                       (< rnd 2) D_S
                       (< rnd 3) D_E
                       (< rnd 4) D_W
                       last-dir))
        (set cur (or (coord-add cur new-dir) cur))
        (set run (math/round (% (* (math/random) 10) 4)))
        (set last-dir new-dir))
      (set cur (or (coord-add cur last-dir) cur))))

  (fill-random-circles 10 10 :floor)
 
  # Postprocessing
  #    - Fill in edges
  #    - Add doors 
  (loop [y :range [0 HEIGHT]]
    (loop [x :range [0 WIDTH]]
      (if (or (= x 0) (= y 0) (= x (- WIDTH 1)) (= y (- HEIGHT 1)))
        (set ((at-dungeon [y x]) :type) :wall))

      (if (= ((at-dungeon [y x]) :type) :floor)
        (do
          (var pattern @[])
          (loop [direction :in directions]
            (def new (coord-add [y x] direction))
            (if new
              (array/push pattern (match ((at-dungeon new) :type)
                                    (@ :floor) "."
                                    _           "#"))
              (array/push pattern "#")))

          # convert array to tuple so we can cmp it with another tuple
          (var pattern (tuple/slice pattern 0))

          (if (or (= pattern ["#" "#" "." "." "." "#" "#" "#"])
                  (= pattern ["." "#" "#" "." "." "#" "#" "#"])
                  (= pattern ["#" "#" "#" "." "." "#" "#" "."])
                  (= pattern ["#" "#" "#" "." "." "." "#" "#"])
                  (= pattern ["#" "#" "." "." "." "#" "#" "."])
                  (= pattern ["." "#" "#" "." "." "." "#" "#"])
                  (= pattern ["." "." "#" "#" "#" "#" "." "#"])
                  (= pattern ["#" "." "." "#" "#" "#" "." "#"])
                  (= pattern ["#" "." "#" "#" "#" "." "." "#"])
                  (= pattern ["#" "." "#" "#" "#" "#" "." "."])
                  (= pattern ["." "." "#" "#" "#" "." "." "#"])
                  (= pattern ["#" "." "." "#" "#" "#" "." "."]))
            (if (< 0.75 (math/random))
              (set ((at-dungeon [y x]) :type) :closed-door))))))))

(defn tick []
  (loop [mob :in mobs]
    (set (mob :fov) (shadowcast (mob :coord) (mob :fov-radius))))
  (loop [coord :keys ((mobs player) :fov)]
    (set (memory coord) true))

  (var dead-fuses @[]) 
  (var fuse-i 0) 
  (loop [fuse :in fuses]
    (if (= 0 (fuse 0))
      (do
        ((fuse 1) (fuse 2))
        (array/push dead-fuses fuse-i)))
    (-- (fuse 0))
    (++ fuse-i))
  
  (loop [dead-fuse :in dead-fuses]
    (array/remove fuses dead-fuse)))

(defn init []
  (load-sprites sprites)

  (loop [y :range [0 HEIGHT]]
    (put dungeon y (array/new WIDTH))
    (loop [x :range [0 WIDTH]]
      (put (get dungeon y) x (table/setproto @{} tile-proto))))

  (set player (length mobs))
  (def player-coord [(/ WIDTH 2) (/ HEIGHT 2)])
  (def player-obj @{ :id player
                     :tile "@"
                     :coord player-coord
                     })
  (array/push mobs (table/setproto player-obj mob-proto))
  (set ((at-dungeon player-coord) :mob) player)

  (mapgen)

  (tick)
  (draw))

(defn step [])

(defn keydown [k]
  (cond
    (or (= k "h") (= k "left"))  (:move-mob (mobs player) D_W)
    (or (= k "j") (= k "down"))  (:move-mob (mobs player) D_S)
    (or (= k "k") (= k "up"))    (:move-mob (mobs player) D_N)
    (or (= k "l") (= k "right")) (:move-mob (mobs player) D_E)
        (= k "y")                (:move-mob (mobs player) D_NW)
        (= k "u")                (:move-mob (mobs player) D_NE)
        (= k "b")                (:move-mob (mobs player) D_SW)
        (= k "n")                (:move-mob (mobs player) D_SE))
  (tick)
  (draw))

(defn mouse [type evnum x y]
  (if (= type "left")
    (do
      (def cell-x (+ startx (math/round (/ x 7 scale))))
      (def cell-y (+ starty (math/round (/ y 7 scale))))
      (if (or (((mobs player) :fov) [cell-y cell-x])
              (memory [cell-y cell-x]))
        (set user-goto (astar ((mobs player) :coord)
                              [cell-y cell-x] manhattan-distance true)))
      (draw))))
