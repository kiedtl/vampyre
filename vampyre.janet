(def title "Vampyre")
(def debug false)

(def height (if debug 100 28))
(def width  (if debug 100 28))
(def scale  (if debug   1  3))

(def HEIGHT 100)
(def WIDTH  100)

(def sprites [
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
    ]
])

(var msg "chilly...")

(def T_WALL  0)
(def T_FLOOR 1)
(def tile-proto @{ :type T_WALL
                   :mob -1
                   :walkable? (fn [tile]
                                (and
                                  (= (tile :type) T_FLOOR)
                                  (= (tile :mob) -1)))
                 })
(var dungeon (array/new HEIGHT))

(def D_N  @{ :x  0 :y -1 })
(def D_S  @{ :x  0 :y  1 })
(def D_E  @{ :x  1 :y  0 })
(def D_W  @{ :x -1 :y  0 })
(def D_NW @{ :x -1 :y -1 })
(def D_NE @{ :x  1 :y -1 })
(def D_SW @{ :x -1 :y  1 })
(def D_SE @{ :x  1 :y  1 })

(var coord-proto @{ :x 0 :y 0 :z 0 })
(defn new-coord [x y z]
  (table/setproto @{ :x x :y y :z z} coord-proto))
(defn eq-coord [a b]
  (and (= (a :x) (b :x)) (= (a :y) (b :y)) (= (a :z) (b :z))))
(defn add-xy [a b]
  (var r @{ :x (+ (a :x) (b :x)) :y (+ (a :y) (b :y)) })
  (if (or (< (r :x) 0) (< (r :y) 0) (>= (r :x) WIDTH) (>= (r :y) HEIGHT))
    nil
    (table/setproto r coord-proto)))
(defn at-dungeon [coord]
  ((dungeon (coord :y)) (coord :x)))

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
                              (if (and dst (:walkable? (at-dungeon dst)))
                                (do
                                  (set ((at-dungeon src) :mob) -1)
                                  (set ((at-dungeon dst) :mob) (mob :id))
                                  (set (mob :coord) dst)
                                  true)
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

(defn // [arg1 & args]
  (var accm arg1)
  (each arg args
    (/= accm arg))
  (math/round accm))

(defn bresenham-circle [center radius]
  (defn add-coord [buf x y]
    (if (and (>= x 0) (>= y 0) (< x WIDTH) (< y HEIGHT))
      (array/push buf [y x])))

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

(defn draw []
  (fill 0 0 width height " ")

  (def player-coord ((mobs player) :coord))

  (def mapwidth (- width 1))
  (def mapheight (- height 2))

  (def startx (max 0 (- (player-coord :x) (//  mapwidth 2))))
  (def starty (max 0 (- (player-coord :y) (// mapheight 2))))
  (def endx (min  WIDTH (+ (player-coord :x) (//  mapwidth 2))))
  (def endy (min HEIGHT (+ (player-coord :y) (// mapheight 2))))

  (var y 0)
  (loop [my :range [starty endy]]
    (var x 0)
    (loop [mx :range [startx endx]]
      (def tile (at-dungeon (new-coord mx my 0)))

      (color
        (if (((mobs player) :fov) [my mx])
          (match (tile :type)
            (@ T_WALL)  0x0D
            (@ T_FLOOR) 0x0E)
          (if (memory [my mx])
            0x0F
            (if debug 0x0F 0x00))))

      (var s
        (match (tile :type)
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
  (c7put 0 (- height 1) msg))

(defn mapgen []
  (var cur (new-coord (/ WIDTH 2) (/ HEIGHT 2) 0))
  (var run 1)
  (var last-dir D_N)

  (loop [_ :range [0 6000]]
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
  
  (loop [_ :range [0 15]]
    (def rx (math/round (% (* (math/random) 1000) WIDTH)))
    (def ry (math/round (% (* (math/random) 1000) HEIGHT)))
    (def max-radius (+ 2 (math/round (% (* (math/random) 1000) 10))))

    (loop [radius :range [1 max-radius]]
      (def circle (bresenham-circle (new-coord rx ry 0) radius))
      (loop [coord :in circle]
        (set ((at-dungeon (new-coord (coord 0) (coord 1) 0)) :type) T_FLOOR)))))

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
                (not= ((at-dungeon coord) :type) T_FLOOR)
                (not  (eq-coord coord center)))
            (set ray-dead true)))
        (set ray-dead true))))
  res)

(defn tick []
  (loop [mob :in mobs]
    (set (mob :fov) (raycast (mob :coord) (mob :fov-radius))))
  (loop [coord :keys ((mobs player) :fov)]
    (set (memory coord) true)))

(defn init []
  (load-sprites sprites)

  (loop [y :range [0 HEIGHT]]
    (put dungeon y (array/new WIDTH))
    (loop [x :range [0 WIDTH]]
      (put (get dungeon y) x (table/setproto @{} tile-proto))))
  (mapgen)

  (set player (length mobs))
  (def player-coord (new-coord (/ WIDTH 2) (/ HEIGHT 2) 0))
  (def player-obj @{ :id player
                     :tile "@"
                     :coord player-coord
                     })
  (array/push mobs (table/setproto player-obj mob-proto))
  (set ((at-dungeon player-coord) :mob) player)

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
