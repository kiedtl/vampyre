(def title "Vampyre")
(def height 28)
(def width 28)
(def scale 3)

(def HEIGHT 100)
(def WIDTH  100)

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

(def D_N @{ :x  0 :y -1 })
(def D_S @{ :x  0 :y  1 })
(def D_E @{ :x  1 :y  0 })
(def D_W @{ :x -1 :y  0 })

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

(defn draw []
  (fill 0 0 width height " ")

  (def player-coord ((mobs player) :coord))

  (def startx (max 0 (- (player-coord :x) (/  width 2))))
  (def starty (max 0 (- (player-coord :y) (/ height 2))))
  (def endx (min  WIDTH (+ (player-coord :x) (/  width 2))))
  (def endy (min HEIGHT (+ (player-coord :y) (/ height 2))))

  (var y 0)
  (loop [my :range [starty endy]]
    (var x 0)
    (loop [mx :range [startx endx]]
      (color
        (if (((mobs player) :fov) [my mx])
          0x0D
          (if (memory [my mx])
            0x0F
            0x00)))
      (def tile (at-dungeon (new-coord mx my 0)))

      (var s (if (= (tile :type) T_WALL) "#" "."))
      (if (not= (tile :mob) -1)
        (set s ((mobs (tile :mob)) :tile)))

      (c7put x y s)
      (++ x))
    (++ y)))

(defn mapgen []
  (var cur (new-coord (/ WIDTH 2) (/ HEIGHT 2) 0))
  (var last-dir D_N)

  (loop [_ :range [0 5000]]
    (set ((at-dungeon cur) :type) T_FLOOR)
    (def rnd (% (* (math/random) 100) 6))
    (def new-dir (cond
                   (< rnd 1) D_N
                   (< rnd 2) D_S
                   (< rnd 3) D_E
                   (< rnd 4) D_W
                   last-dir))
    (set cur (or (add-xy cur new-dir) cur))
    (set last-dir new-dir)))

(defn raycast [center radius]
  (var res @{})
  (set (res [(center :y) (center :x)]) true)

  (loop [i :range [0 360]]
    (def ax (math/sin (/ (* i math/pi) 180)))
    (def ay (math/cos (/ (* i math/pi) 180)))

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
                (not (:walkable? (at-dungeon coord)))
                (not (eq-coord coord center)))
            (set ray-dead true)))
        (set ray-dead true))))
  res)

(defn tick []
  (loop [mob :in mobs]
    (set (mob :fov) (raycast (mob :coord) (mob :fov-radius))))
  (loop [coord :keys ((mobs player) :fov)]
    (set (memory coord) true)))

(defn init []
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
    (or (= k "l") (= k "right")) (:move-mob (mobs player) D_E))
  (tick)
  (draw))
