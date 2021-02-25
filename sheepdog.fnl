;; title:  Sheepdog
;; author: Stephen Wassell
;; desc:   short description
;; script: fennel

(macro xy [x y] `{:x ,x :y ,y})

;; (fn setxy [dst src]
;;   (set dst.x src.x)
;;   (set dst.y src.y))

(fn xy* [v m] (xy (* v.x m) (* v.y m)))
(fn xy/ [v d] (xy (/ v.x d) (/ v.y d)))
(fn xy+ [a b] (xy (+ a.x b.x) (+ a.y b.y)))
(fn xy- [a b] (xy (- a.x b.x) (- a.y b.y)))
(fn xy0? [v] (= v.x v.y 0))

(fn magnitude [v]
  "Return the magnitude of a vector."
  (math.sqrt (+ (* v.x v.x) (* v.y v.y))))

(fn normalise [v mag]
  "Normalise vector to a given magnitude."
  (let [scale (magnitude v)]
    (if (< scale .1) (xy 0 0) ; Avoid divide by 0 and excessive wiggling.
      (xy* (xy/ v scale) mag))))

(fn xy-random []
  "Return a random vector with x and y in the range -.5 to .5."
  (xy (- (math.random) .5) (- (math.random) .5)))

; TIC-80 screen size.
(local screen-w 240)
(local screen-h 136)
; Reduced by the size of a sprite.
(local screen-w-s (- screen-w 8))
(local screen-h-s (- screen-h 8))

; Background of the map and transparent sprites - dark green.
(local bg-colour 5)

; Incremented on each frame.
(var t 0)

; Will be updated on draw to the average sheep location.
(var herd-center (xy 120 60))

; How many to create.
(var sheep-count 20)

; Set of ids of things that scare the sheep - eg player.
(var scary-ids {})

; Tables of entity id => closure. All entries in update and draw are called on each frame.
(var fns-move-away {}) ; id (fn [from]) -> vector to move away from 'from'
(var fns-update {}) ; id (fn []) -> action vector
(var fns-draw {}) ; id (fn [])

(macro ++ [n]
       "Increment n and return the new value."
       `(do
          (set ,n (+ ,n 1))
          ,n))

(macro *= [a b]
       `(do
          (set ,a (* ,a ,b))
          ,a))

(var draw-map nil)

((fn []
   "Initialise the map and its methods."
   (set draw-map (fn [] (map)))))

(global TIC
        (fn tic []
          "The main game loop. On each frame call all updaters then all drawers."
          (each [_ f (pairs fns-update)] (f))
          
          (draw-map)
          
          (set herd-center (xy 0 0))
          (each [_ f (pairs fns-draw)] (f))
          (set herd-center (xy/ herd-center sheep-count))
          
          (++ t)))

(local unique-id ((fn [] ; Note double ( to call the closure immediately.
                    "Return a closure which increments id and returns the new value. Call in constructors."
                    (var id 0)
                    (fn [] (++ id)))))

(fn buttons []
  "List of 0/1 for buttons pressed: up down left right"
  (values
   (if (btn 0) 1 0)
   (if (btn 1) 1 0)
   (if (btn 2) 1 0)
   (if (btn 3) 1 0)))

(fn to-mouse [from]
  "Return vector from x y to mouse, if any buttons pressed (else 0 0)."
  (local (mouse-x mouse-y left middle right scrollx scrolly) (mouse))
  (if (or left middle right) (xy (- mouse-x from.x) (- mouse-y from.y))
    (xy 0 0)))

(fn get-action [from accel]
  "Return direction to move player based on buttons and mouse, normalised to magnitude accel."
  (let [dir (match (buttons)
                   ; up down left right
                   (1 0 0 0) (xy 0 -1)
                   (0 1 0 0) (xy 0 1)
                   (0 0 1 0) (xy -1 0)
                   (0 0 0 1) (xy 1 0)
                   (1 0 1 0) (xy -1 -1)
                   (1 0 0 1) (xy 1 -1)
                   (0 1 1 0) (xy -1 1)
                   (0 1 0 1) (xy 1 1)
                   _ (to-mouse from))] ; No buttons pressed, go towards mouse.
    (normalise dir accel)))

(fn center [pos]
  "The center of a sprite."
  (xy (+ pos.x 4) (+ pos.y 4)))

(fn alternate [s id]
  "Alternate between s and s+1 for running feet and wagging tails."
  (+ s (/ (% (+ t (* id 10)) 24) 12)))

(fn moving [d]
  "True if we need the sprite for moving, given pixels per frame."
  (or (> (math.abs d.x) .1) (> (math.abs d.y) .1)))

(fn stay-in-field [pos vel]
  "Call before updating pos to keep it in bounds."
  (when (or (< pos.x 0) (> pos.x screen-w-s)) (*= vel.x -1.5))
  (when (or (< pos.y 0) (> pos.y screen-h-s)) (*= vel.y -1.5)))

(fn move-away [from me scariness]
  "Return a vector to move away from 'from', scaled by distance and scariness, 0 if too far away.
  Include some randomness to avoid them getting stuck together."
  (let [away (xy+ (xy* (xy-random) 5) (xy- from me))
        mag (magnitude away)]
    (if (> mag scariness) (xy 0 0) ; too far away
      (< mag 1) (xy-random) ; too close
      (normalise away (/ scariness mag)))))

(fn move-away-from-all [me id]
  "Sum the move-away vectors for all nearby entities."
  (var action (xy 0 0))
  (var scared false)
  (each [it f (pairs fns-move-away)]
        (when (~= it id) ; it's not me
          (let [vec (f me)] ; move away from it by this vector
            (when (not (xy0? vec)) ; close enough to move away from
              (set action (xy+ action (f me)))
              (when (. scary-ids it) (set scared true))))))
  (values action scared))

(fn entity-template [init update post-draw]
  "Return a function which will create an entity when called.
  Adds a closure to each of the fns- tables defined above.

  The caller must provide 3 functions, each in the form (fn [self] ...):
  init: called once when creating the new entity.
  update: called on update, return an action vector. May be nil if it doesn't move.
  post-draw: called after drawing the sprite at self.pos. May be nil."
  (fn []
    (var self {
               :id (unique-id)
               
               ; Current location, updated in drawer.
               :pos (xy 0 0)
               
               ; Current velocity in pixels per frame, updated in updater.
               :vel (xy 0 0)
               
               ; Images face left. Set to 1 when moving right to flip the sprite.
               :flip 0
               
               ; To avoid flipping too rapidly, this is set to the time when we're
               ; next allowed to change flip.
               :flip-time 0
               
               ; How fast can it run.
               :accel .15
               :friction .9
               
               ; Sprite indices, also +1 to each for alternate.
               :spr-run 272
               :spr-idle 274
               :sprite spr-idle
               
               ; Only change to idle sprite after a moment of not moving.
               :idle-time 0
               
               ; How fast sheep need to run away.
               :scariness 10 } )
    
    ; Call the provided initialiser to set the above values if required.
    (init self)
    
    (when (~= 0 self.scariness)
      (tset fns-move-away self.id
            (fn [from]
              "Return a vector to move away from this entity."
              (move-away from self.pos self.scariness))))
    
    (when (~= nil update)
      (tset fns-update self.id
            (fn []
              "Update vel from the action vector returned by update."
              (var action (update self))
              (when (and (~= action.x 0) (> t self.flip-time))
                (set self.flip (if (> action.x 0) 1 0))
                ; Don't change flip again for a moment.
                (set self.flip-time (+ t 10)))
              (set self.sprite
                   (if (and (xy0? action) (> t self.idle-time)) self.spr-idle self.spr-run))
              ; Don't use idle sprite until not moving for a moment.
              (when (not (xy0? action)) (set self.idle-time (+ t 20)))
              (set self.vel (xy* (xy+ self.vel action) self.friction)))))
    
    (tset fns-draw self.id
          (fn []
            "Update pos from vel, and draw the sprite at pos."
            (stay-in-field self.pos self.vel)
            (set self.pos (xy+ self.pos self.vel))
            (spr (alternate self.sprite self.id)
                 self.pos.x self.pos.y
                 bg-colour 1 self.flip)
            (when (~= nil post-draw) (post-draw self))))))

(local new-player (entity-template
                   ; init
                   (fn [self]
                     (set self.pos (xy (/ screen-w-s 2) (/ screen-h-s 2)))
                     (set self.spr-run 272)
                     (set self.spr-idle 274)
                     (set self.scariness 50)
                     (tset scary-ids self.id 1))
                   ; update -> action
                   (fn [self]
                     (var (action scared) (move-away-from-all self.pos self.id))
                     (xy+
                      (get-action (center self.pos) self.accel)
                      (normalise action .5)))
                   ; post-draw
                   nil))

(local new-sheep (entity-template
                  ; init
                  (fn [self]
                    (set self.pos (xy (math.random screen-w-s) (math.random screen-h-s)))
                    (set self.spr-run 256)
                    (set self.spr-idle 258))
                  ; update -> action
                  (fn [self]
                    (var (action scared) (move-away-from-all self.pos self.id))
                    ; If moving away from player, head towards the center of the herd.
                    (when scared
                      (set action (xy+ action (normalise (xy- herd-center self.pos) 1))))
                    (normalise action self.accel))
                  ; post-draw
                  (fn [self]
                    (set herd-center (xy+ herd-center self.pos)))))

; Create the persistent entities.
(new-player)
(for [_ 1 sheep-count] (new-sheep))

;; <TILES>
;; 000:5555555555555555555555555555555555555555555555555555555555555555
;; 001:55555555555555555555555555f555555555555555555f555555555555555555
;; 002:55555555555555555555f55555f5555555555555555555555555f55555555555
;; 003:555555555555555555555e5555e5555555555555555555555555555555555555
;; 004:5555555555555555555e555555555555555555555555e5555555555555555555
;; 005:55555555555555555555555b555b55b555b555b555b555555555555555555555
;; 006:555555555555555555555555555b555555b555b555b55b5555555b5555555555
;; 007:5555555555555555555555555555555555555555555555555555555555555555
;; 008:5555555555555555555555555555555555555555555555555555555555555555
;; 009:5555555555555555555555555555555555555555555555555555555555555555
;; 010:5555555555555555555555555555555555555555555555555555555555555555
;; 011:5555555555555555555555555555555555555555555555555555555555555555
;; 012:5555555555555555555555555555555555555555555555555555555555555555
;; 013:5555555555555555555555555555555555555555555555555555555555555555
;; 014:5555555555555555555555555555555555555555555555555555555555555555
;; 015:5555555555555555555555555555555555555555555555555555555555555555
;; </TILES>

;; <SPRITES>
;; 000:5ffff555f0f0ff55fffffcf55fffccff55cccfff55ffffff555fffff55550505
;; 001:5ffff555f0f0ff55fffffcf55fffccff55cccfff55ffffff555fffff55505550
;; 002:55555555555555555555fff5555fcfff5ffffcfffffffcfff0f0ffff5fff0505
;; 003:55555555555555555555fff55fffcffffffffcfff0f0fcff5fffffff55550505
;; 016:5505055555000555c0d0d055c0000f0555fff00055000005555000055555f5f5
;; 017:5505055555000555c0d0d055c0000f0555fff0005500000555500005555f555f
;; 018:5505055555000555c0d0d055c0000f0555fff00055000005555000055555f5f5
;; 019:5505055555000555c0d0d055c0000f0555fff00555000000555000055555f5f5
;; </SPRITES>

;; <MAP>
;; 001:002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <PALETTE>
;; 000:140c1c44243430346d4e4a4e854c30346524d04648757161597dced27d2c8595a16daa2cd2aa996dc2cadad45edeeed6
;; </PALETTE>

