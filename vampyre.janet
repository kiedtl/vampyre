(def title "Vampyre")
(def debug false)

(def height (if debug 110 28))
(def width  (if debug 110 28))
(def scale  (if debug   1  3))

(def HEIGHT 100)
(def WIDTH  100)

(def sprites [
    [ "["
      1 1 1 1 1 1 0
      1 1 0 1 0 1 0
      1 1 0 1 0 1 0
      1 1 0 1 0 1 0
      1 1 0 1 0 1 0
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

(def T_WALL  0)
(def T_FLOOR 1)
(def T_DOORC 2)
(def T_DOORO 3)
(def tile-proto @{ :type T_WALL
                   :mob -1
                   :walkable? (fn [tile]
                                (and
                                  (or
                                    (= (tile :type) T_FLOOR)
                                    (= (tile :type) T_DOORO))
                                  (= (tile :mob) -1)))
                 })
(var dungeon (array/new HEIGHT))

(var fuses @[])

(def D_N  @{ :x  0 :y -1 })
(def D_S  @{ :x  0 :y  1 })
(def D_E  @{ :x  1 :y  0 })
(def D_W  @{ :x -1 :y  0 })
(def D_NW @{ :x -1 :y -1 })
(def D_NE @{ :x  1 :y -1 })
(def D_SW @{ :x -1 :y  1 })
(def D_SE @{ :x  1 :y  1 })

(def directions [ [ -1  -1 ] # NW
                  [ -1   0 ] # N
                  [ -1   1 ] # NE
                  [  0  -1 ] # W
                  [  0   1 ] # E
                  [  1  -1 ] # SW
                  [  1   0 ] # S
                  [  1   1 ] # SE
                ])

(var coord-proto @{ :x 0 :y 0 :z 0 })
(defn new-coord [x y z]
  (table/setproto @{ :x x :y y :z z} coord-proto))

(defn eq-coord [a b]
  (and (= (a :x) (b :x)) (= (a :y) (b :y)) (= (a :z) (b :z))))

(defn coord-valid? [c]
  (and (>= (c 0) 1) (>= (c 1) 1)
       (< (c 1) (- WIDTH 1)) (< (c 0) (- HEIGHT 1))))

(defn coord-add-xy2 [a b]
  (def r [(+ (a 0) (b 0)) (+ (a 1) (b 1))])
  (if (not (coord-valid? r)) nil r))

(defn add-xy [a b]
  (var r @{ :x (+ (a :x) (b :x)) :y (+ (a :y) (b :y)) })
  (if (or (< (r :x) 1) (< (r :y) 1)
          (>= (r :x) (- WIDTH 1)) (>= (r :y) (- HEIGHT 1)))
    nil
    (table/setproto r coord-proto)))

(defn manhattan-distance [a b]
  (def diff [ (math/abs (- (a 0) (b 0)))
              (math/abs (- (a 1) (b 1)))
            ])
  (+ (diff 0) (diff 1)))

(defn at-dungeon [coord]
  ((dungeon (coord :y)) (coord :x)))

(defn at-dungeon2 [coord]
  ((dungeon (coord 0)) (coord 1)))

(def mob-proto @{ :id -1
                  :tile "g"
                  :coord (new-coord -1 -1 0)
                  :max-hp 10
                  :hp 10
                  :fov-radius (- (/ (max height width) 2) 3)
                  :fov @{}
                  :move-mob (fn [mob dir]
                              (def src (mob :coord))
                              (def dst (add-xy src dir))
                              (if dst
                                (do
                                  (def tile (at-dungeon dst))
                                  (cond
                                    (= (tile :type) T_DOORC)
                                      (do
                                        (set (tile :type) T_DOORO)
                                        (array/push fuses @[4 (fn [c]
                                                               (set (tile :type) T_DOORC))
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

(defn bresenham-circle [center radius]
  (defn add-coord [buf x y]
    (if (coord-valid? [y x]) (array/push buf [y x])))

  (var res @[])

  (def circum (math/ceil (* (* math/pi 2) radius)))
  (def x (center :x))
  (def y (center :y))

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
    (def coord (new-coord (self 1) (self 0) 0))
    (and (or
           (:walkable? (at-dungeon coord))
           (= ((at-dungeon coord) :type) T_DOORC))
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
      (def neighbor (coord-add-xy2 (cur :coord) direction))
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

  (set startx (max 0 (- (player-coord :x) (//  mapwidth 2))))
  (set starty (max 0 (- (player-coord :y) (// mapheight 2))))
  (set endx (min  WIDTH (+ (player-coord :x) (//  mapwidth 2))))
  (set endy (min HEIGHT (+ (player-coord :y) (// mapheight 2)))))

(defn draw []
  (fill 0 0 width height " ")
  (calc-display-offsets)

  (var y 0)
  (loop [my :range [starty endy]]
    (var x 0)
    (loop [mx :range [startx endx]]
      (def tile (at-dungeon (new-coord mx my 0)))

      (color
        (if (((mobs player) :fov) [my mx])
          (match (tile :type)
            (@ T_WALL)  0x0D
            (@ T_DOORC) 0x0C
            (@ T_DOORO) 0x0C
            (@ T_FLOOR) 0x0E)
          (if (memory [my mx])
            0x0F
            (if debug 0x0F 0x00))))

      (var s
        (match (tile :type)
          (@ T_DOORC) "["
          (@ T_DOORO) "|"
          (@ T_WALL)  "#"
          (@ T_FLOOR) "*"))

      (if (not= (tile :mob) -1)
        (do
          (set s ((mobs (tile :mob)) :tile))
          (color 0x01)))

      (c7put x y s)
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
        (if (= ((at-dungeon2 coord) :type) T_FLOOR)
          (c7put dx dy "&")))
      (set user-goto nil))))

(defn mapgen []
  (defn fill-random-circles [num max-radius tiletype]
    (loop [_ :range [0 num]]
      (def rx (math/round (% (* (math/random) 1000) WIDTH)))
      (def ry (math/round (% (* (math/random) 1000) HEIGHT)))
      (def radius (+ 2 (math/round (% (* (math/random) 1000) max-radius))))

      (loop [r :range [1 max-radius]]
        (def circle (bresenham-circle (new-coord rx ry 0) r))
        (loop [coord :in circle]
          (set ((at-dungeon2 coord) :type) tiletype)))))

  (defn dig-maze [coord]
    (def neighbors @[ [[ -1  0 ] [ -2  0 ]]
                      [[  1  0 ] [  2  0 ]]
                      [[  0  1 ] [  0  2 ]]
                      [[  0 -1 ] [  0 -2 ]]
                    ])
    (shuffle neighbors)
    (loop [neighbor :in neighbors]
      (def icoord (coord-add-xy2 coord (neighbor 0)))
      (def ncoord (coord-add-xy2 coord (neighbor 1)))
      (if (and icoord ncoord
               (not= ((at-dungeon2 ncoord) :type) T_FLOOR))
        (do
          (set ((at-dungeon2 icoord) :type) T_FLOOR)
          (set ((at-dungeon2 ncoord) :type) T_FLOOR)
          (dig-maze ncoord)))))

  # Disabled for now.
  #(dig-maze [0 0])
  #(fill-random-circles 50 9 T_WALL)

  (var cur (new-coord (/ WIDTH 2) (/ HEIGHT 2) 0))
  (var run 1)
  (var last-dir D_N)

  (loop [_ :range [0 10000]]
    (set ((at-dungeon cur) :type) T_FLOOR)
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
        (set cur (or (add-xy cur new-dir) cur))
        (set run (math/round (% (* (math/random) 10) 4)))
        (set last-dir new-dir))
      (set cur (or (add-xy cur last-dir) cur))))

  (fill-random-circles 10 10 T_FLOOR)
  
  (loop [y :range [0 HEIGHT]]
    (loop [x :range [0 WIDTH]]
      (if (= ((at-dungeon2 [y x]) :type) T_FLOOR)
        (do
          (var pattern @[])
          (loop [direction :in directions]
            (def new (coord-add-xy2 [y x] direction))
            (if new
              (array/push pattern (match ((at-dungeon2 new) :type)
                                    (@ T_FLOOR) "."
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
              (set ((at-dungeon2 [y x]) :type) T_DOORC))))))))

(def raycast-sin-vals
  (map (fn [i] (math/sin (/ (* i math/pi) 180))) (range 0 360)))

(def raycast-cos-vals
  (map (fn [i] (math/cos (/ (* i math/pi) 180))) (range 0 360)))

(defn raycast [center radius]
  (var res @{})
  (set (res [(center :y) (center :x)]) true)

  (loop [i :range [0 360 2]]
    (def ax (raycast-sin-vals i))
    (def ay (raycast-cos-vals i))

    (var x (center :x))
    (var y (center :y))

    (var ray-dead false)
    (loop [z :range [0 radius] :until ray-dead]
      (+= x ax)
      (+= y ay)

      (def ix (math/round x))
      (def iy (math/round y))
      (def coord (new-coord ix iy (center :z)))

      (if (and (>= ix 0) (>= iy 0) (< ix WIDTH) (< iy HEIGHT))
        (do
          (set (res [iy ix]) true)
          (if (and
                (=   ((at-dungeon coord) :type) T_WALL)
                (not (eq-coord coord center)))
            (set ray-dead true)))
        (set ray-dead true))))
  res)

(defn tick []
  (loop [mob :in mobs]
    (set (mob :fov) (raycast (mob :coord) (mob :fov-radius))))
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
  (def player-coord (new-coord (/ WIDTH 2) (/ HEIGHT 2) 0))
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
        (do
          (def player-coord ((mobs player) :coord))
          (set user-goto (astar [(player-coord :y) (player-coord :x)]
                                [cell-y cell-x] manhattan-distance true))))
      (draw))))
