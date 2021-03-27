;; title:  One Man And His Dog
;; author: Stephen Wassell
;; desc:   Art by u/please_send_cookies
;; script: fennel

; Constants and globals.

; TIC-80 screen size.
(local screen-w 240)
(local screen-h 136)
; Reduced by the size of a sprite.
(local screen-w-s (- screen-w 8))
(local screen-h-s (- screen-h 8))
; Center of the screen.
(local center-x (/ screen-w 2))
(local center-y (/ screen-h 2))

; Background of the map and transparent sprites - dark green.
(local bg-colour 5)

; Incremented on each frame.
(var t 0)

; Set of ids of things that scare the sheep - eg player.
(var scary-ids {})

; Tables of entity id => closure. All entries in update and draw are called on each frame.
(var fns-move-away {}) ; id (fn [from]) -> vector to move away from 'from'
(var fns-update []) ; (fn []) -> action vector
(var fns-draw []) ; (fn [callback])

; Some utility functions.

(fn tidy-up []
  "Initialise globals at the start of a new level."
  (set scary-ids {})
  (set fns-move-away {})
  (set fns-update [])
  (set fns-draw [])
  (collectgarbage))

(macro ++ [n]
       "Increment n and return the new value."
       `(do
          (set ,n (+ ,n 1))
          ,n))

(macro += [a b]
       "Set a to a+b and return a."
       `(do
          (set ,a (+   ,a ,b))
          ,a))

(macro *= [a b]
       "Set a to a*b and return a."
       `(do
          (set ,a (* ,a ,b))
          ,a))

(local unique-id ((fn [] ; Note double ( to call the closure immediately.
                    "Return a closure which increments id and returns the new value. Call in constructors."
                    (var id 0)
                    (fn [] (++ id)))))

; Vector arithmetic.

(macro xy [x y]
       "Create a new vector. Vectors are tables containing :x and :y values."
       `{:x ,x :y ,y})

(fn xy* [v m]
  "Return a vector with both values multiplied by a scalar."
  (xy (* v.x m) (* v.y m)))

(fn xy/ [v d]
  "Return a vector with both values divided by a scalar."
  (xy (/ v.x d) (/ v.y d)))

(fn xy+ [a b]
  "Add two vectors."
  (xy (+ a.x b.x) (+ a.y b.y)))

(fn xy- [a b]
  "Subtract two vectors."
  (xy (- a.x b.x) (- a.y b.y)))

(fn xy0? [v]
  "Return true if both values are 0."
  (= v.x v.y 0))

(fn magnitude [v]
  "Return the magnitude of a vector."
  (math.sqrt (+ (* v.x v.x) (* v.y v.y))))

(fn normalise [v mag]
  "Normalise vector to a given magnitude."
  (let [scale (magnitude v)]
    (if (< scale .1) (xy 0 0) ; Avoid divide by 0 and excessive wiggling.
      (xy* (xy/ v scale) mag))))

(fn center [pos]
  "The center of an 8x8 sprite."
  (xy (+ pos.x 4) (+ pos.y 4)))

; Some functions to return random values.

(fn xy-random []
  "Return a random vector with x and y in the range -.5 to .5."
  (xy (- (math.random) .5) (- (math.random) .5)))

(fn pick-random [array]
  "Return a randomly selected item from the provided array."
  (. array (math.random (length array))))

(fn random0 [max]
  "Return a random number in the range 0 <= n < max."
  (- (math.random max) 1))

; Input handling.

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

; Sprite selection.

(fn alternate [s id]
  "Alternate between s and s+1 for running feet and wagging tails."
  (+ s (/ (% (+ t (* id 10)) 24) 12)))

(fn moving [d]
  "True if we need the sprite for moving, given pixels per frame."
  (or (> (math.abs d.x) .1) (> (math.abs d.y) .1)))

; Hard collision detection for the field boundary and solid map objects.

(fn solid [x y]
  "Return true if the location is in a solid object on the map, eg fences."
  (local map-x (/ x 8))
  (local map-y (/ y 8))
  (or (fget (mget map-x map-y) 0)
      (fget (mget (+ .9 map-x) map-y) 0)
      (fget (mget (+ .9 map-x) (+ .9 map-y)) 0)
      (fget (mget map-x (+ .9 map-y)) 0)))

(fn find-random-space []
  "Return a random location that's not over a solid object on the map.
  Used to place new sheep."
  (local ret (xy (math.random screen-w-s) (math.random screen-h-s)))
  (if (solid ret.x ret.y)
    (find-random-space)
    ret))

(fn stay-in-field [pos vel]
  "Call before updating pos to keep it in bounds and off solid map objects.
  Modifies vel. This is a hard collision.
  
  First check collisions due to x and y movement separately, so the entity
  can glide along the obstacle rather than get stuck on it. The final check
  on the updated position is to deal with convex corners."
  (local new-x (+ pos.x vel.x))
  (local new-y (+ pos.y vel.y))
  (when (or
         (< new-x 0)
         (> new-x (+ 1 screen-w-s))
         (solid new-x pos.y))
    (set vel.x 0))
  (when (or
         (< new-y 0)
         (> new-y (+ 1 screen-h-s))
         (solid pos.x new-y))
    (set vel.y 0))
  (when (solid (+ pos.x vel.x) (+ pos.y vel.y))
    (set vel.x 0)
    (set vel.y 0)))

; Soft collision detection between the sheep and dog.

(fn move-away [from me scariness]
  "Return a vector to move away from 'from', scaled by distance and scariness, 0 if too far away.
  Include some randomness to avoid them getting stuck together."
  (local away (xy+ (xy* (xy-random) 5) (xy- from me)))
  (local mag (magnitude away))
  (if (> mag scariness) (xy 0 0) ; too far away
    (< mag 1) (xy-random) ; too close
    (normalise away (/ scariness mag))))

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

; Creating game entities (is that the right word for how I've done it here?).

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
               
               ; How fast sheep need to run away from this entity.
               ; This is right for a sheep; increase for a dog.
               :scariness 10 } )
    
    ; Call the provided initialiser to change any of the above values.
    (init self)
    
    ; Add a closure to fns-move-away if this is a solid object.
    (when (~= 0 self.scariness)
      (tset fns-move-away self.id
            (fn [from]
              "Return a vector to move away from this entity."
              (move-away from self.pos self.scariness))))
    
    ; Optionally add a closure to fns-update.
    (when (~= nil update)
      (table.insert fns-update
                    (fn [herd-center]
                      "Update vel from the action vector returned by update."
                      (var action (update self herd-center))
                      (when (and (~= action.x 0) (> t self.flip-time))
                        (set self.flip (if (> action.x 0) 1 0))
                        ; Don't change flip again for a moment.
                        (set self.flip-time (+ t 10)))
                      (set self.sprite
                           (if (and (xy0? action) (> t self.idle-time))
                             self.spr-idle
                             self.spr-run))
                      ; Don't use idle sprite until not moving for a moment.
                      (when (not (xy0? action)) (set self.idle-time (+ t 20)))
                      (set self.vel (xy* (xy+ self.vel action) self.friction))
                      (stay-in-field self.pos self.vel))))
    
    ; Always add a closure to fns-draw. Return what post-draw returned,
    ; or 0 if nil. This is used for counting the sheep.
    (table.insert fns-draw
                  (fn [callback]
                    "Update pos from vel, and draw the sprite at pos."
                    (set self.pos (xy+ self.pos self.vel))
                    (spr (alternate self.sprite self.id)
                         self.pos.x self.pos.y
                         bg-colour 1 self.flip)
                    (if (~= nil post-draw)
                      (post-draw self callback))))))

; Call this to create a new player dog at the center of the screen.
(local new-player (entity-template
                   ; init
                   (fn init [self]
                     (set self.pos (xy (/ screen-w-s 2) (/ screen-h-s 2)))
                     (set self.spr-run 272)
                     (set self.spr-idle 274)
                     (set self.scariness 50)
                     (tset scary-ids self.id 1))
                   ; update -> action
                   (fn update [self]
                     (var (action scared) (move-away-from-all self.pos self.id))
                     (xy+
                      (get-action (center self.pos) self.accel)
                      (normalise action .5)))
                   ; post-draw
                   nil))

; Call this to create a new sheep at a random location.
(local new-sheep (entity-template
                  ; init
                  (fn init [self]
                    (set self.pos (find-random-space))
                    (set self.spr-run 256)
                    (set self.spr-idle 258))
                  ; update -> action
                  (fn update [self herd-center]
                    (var (action scared) (move-away-from-all self.pos self.id))
                    ; If moving away from player, head towards the center of the herd.
                    (when scared
                      (set action (xy+ action (normalise (xy- herd-center self.pos) 1))))
                    (normalise action self.accel))
                  ; post-draw
                  (fn post-draw [self callback]
                    (callback self.pos))))

(fn decorate-map []
  "Add some flowers to empty space on the map."
  (local decorations [21 34 146 81 87 117])
  ; Decorate empty areas of the map.
  ; todo: increase values when we use more maps
  (for [i 0 20]
    (local x (random0 30))
    (local y (random0 17))
    (when (= 0 (mget x y)) (mset x y (pick-random decorations)))))

(fn play-level [is-in-play?]
  "The main game loop, called from the game coroutine.
  On each frame call all updaters then all drawers.
  When a sheep is drawn it adds its coords to herd-center, which we divide
  by the number of sheep here to calculate the average = coords of the center
  of the herd. Sheep that are scared by the dog move towards that location."
  
  ; Will be updated on draw to the average sheep location.
  (var herd-center (xy 120 60))
  
  (var finished false)
  (while (not finished)
    ; Wait for the engine to call the TIC function.
    (coroutine.yield)
    
    ; Call all updaters - decide where the dog and sheep will move to.
    (each [_ update (ipairs fns-update)]
          (update herd-center))
    
    (set herd-center (xy 0 0))
    (var sheep-count 0)
    
    ; Will remain true only when all sheep are in the finish area.
    (set finished true)
    
    ; Call all drawers. For each sheep, add their coords to herd-center,
    ; and call the closure returned by the level to check if they
    ; are still in play (true) or in the finish area (false).
    (each [_ draw (ipairs fns-draw)]
          (draw (fn [pos]
                  (set herd-center (xy+ herd-center pos))
                  (++ sheep-count) ; zzz
                  (when (is-in-play? pos) (set finished false)))))
    (set herd-center (xy/ herd-center sheep-count))
    
    (++ t)))

(fn button-pressed? []
  "Return true if any mouse or controller button is pressed."
  ; todo: like btnp for mouse click
  (local (mouse-x mouse-y left middle right scrollx scrolly) (mouse))
  (or left middle right (btnp 4) (btnp 5) (btnp 6) (btnp 7)))

(fn print-border [text x y scale]
  "Like print but yellow with a black border."
  (each [_ offset (ipairs [(xy 0 1)
                           (xy 0 -1)
                           (xy 1 0)
                           (xy -1 0)])]
        (print text (+ x offset.x) (+ y offset.y) 0 false (if scale scale 1)))
  (print text x y 2 false (if scale scale 1)))

(fn title []
  ; todo: add auto dog
  (table.insert fns-draw (fn [callback]
                           (map)))
  (for [_ 1 12] (new-sheep))
  (table.insert fns-draw (fn [callback]
                           (print-border "One Man and His Dog" 16 16 2)
                           (print-border "Press X or click to start..." 16 40)))
  ; Return false when we want to go to the next level.
  (fn [] (not (button-pressed?))))

(fn say [message]
  "Return a closure which creates a level that just displays a message."
  (fn []
    (local text (.. message "\n\nPress X or click to continue..."))
    (table.insert fns-draw (fn [callback]
                             (cls 5) ; todo: avoid cls
                             (print-border text 16 16)
                             (callback (xy 0 0))))
    ; Return false when we want to go to the next level.
    (fn [] (not (button-pressed?)))))

(fn outside? [pos center radius]
  "Return true if the sprite at pos is outside the circle defined by center and radius."
  ; todo: check this is right
  (local dx (- pos.x center.x -4))
  (local dy (- pos.y center.y -4))
  (> (+ (* dx dx) (* dy dy)) (* radius radius)))

(fn level1 []
  (local c (xy center-x center-y))
  (local r (/ center-y 2))
  (table.insert fns-draw (fn []
                           (map)
                           (circb c.x c.y r 15)))
  (new-player)
  (for [_ 1 12] (new-sheep))
  ; Return false when this sheep is in the finish area.
  (fn [pos]
    (outside? pos c r)))

(fn game []
  "Coroutine to step through levels and run the main game loop."
  
  (local levels [
                 title
                 (say "Level 1\n\nHerd the sheep into the circle.") level1
                 (say "Well done!\n\nWould you like to play again?")
                 ])
  
  (decorate-map)
  (while true
    (each [_ level (ipairs levels)]
          ; Call the level's closure to initialise and return a closure,
          ; which will be called for each sheep to check if the level is finished.
          (play-level (level))
          (tidy-up))))

; Create a coroutine from the game function and resume it on each frame.
; It needs an initial resume call to get things started.
(local resume-game (coroutine.wrap game))
(resume-game)
(global TIC (fn tic [] (resume-game)))

;; <TILES>
;; 000:5555555555555555555555555555555555555555555555555555555555555555
;; 001:5555555555555555555555555555555555555555555555555555555555555555
;; 002:5555555555555555555555555555555555555555555555555555555555555555
;; 003:5555555555555555555555555555555555555555555555555555555555555555
;; 004:5555555555555555555555555555555555555555555555555555555555555555
;; 005:5555555555555555555555555555555555555555555555555555555555555555
;; 006:5555555555555555555555555555555555555555555555555555555555555555
;; 007:5555555555555555555555555555555555555555555555555555555555555555
;; 008:5555555555555555555555555555555555555555555555555555555555555555
;; 009:5555555555555555555555555555555555555555555555555555555555555555
;; 010:5555555555555555555555555555555555555555555555555555555555555555
;; 011:5555555555555555555555555555555555555555555555555555555555555555
;; 012:5555555555555555555555555555555555555555555555555555555555555555
;; 013:5555555555555555555555555555555555555555555555555555555555555555
;; 014:5555555555555555555555555555555555555555555555555555555555555555
;; 015:5555555555555555555555555555555555555555555555555555555555555555
;; 016:5555555555555555555555555555555555555555555555555555555555555555
;; 017:5555555555555555555555555555555555555555555555555555555555555555
;; 018:5555555555555555555555555555555555555555555555555555555555555555
;; 019:5555555555555555555555555555555555555555555555555555555555555555
;; 020:5555555555555555555555555555555555555555555555555555555555555555
;; 021:5555554555555445555554555444555555444455555445555555555555555555
;; 022:5555555555555555555555555555555555555555555555555555555555555555
;; 023:5555555555555555555555555555555555555555555555555555555555555555
;; 024:5555555555555555555555555555555555555555555555555555555555555555
;; 025:5555555555555555555555555555555555555555555555555555555555555555
;; 026:5555555555555555555555555555555555555555555555555555555555555555
;; 027:5555555555555555555555555555555555555555555555555555555555555555
;; 028:5555555555555555555555555555555555555555555555555555555555555555
;; 029:5555555555555225555521125525622552125555552652555555212555556255
;; 030:5555555555555555555555555555555555555555555555555555555555555555
;; 031:5555550055555500555555005555550055555500555555005555550055555500
;; 032:5555555555555555555555555555555555555555555555555555555555555555
;; 033:5555555555555555555555555555555555555555555555555555555555555555
;; 034:5555555555555555555555445555544554445555554445555555555555555555
;; 035:5555555555555555555555555555555555555555555555555555555555555555
;; 036:5555555555555555555555555555555555555555555555555555555555555555
;; 037:5555555555555555555555555555555555555555555555555555555555555555
;; 038:5555555555555555555555555555555555555555555555555555555555555555
;; 039:5555555555555555555555555555555555555555555555555555555555555555
;; 040:555555555555555555555555555555bb555555bb555555bb555555bb555555bb
;; 041:555555555555555555555555ddbbddbbddbbddbbddbbddbbddbbddbbddbbddbb
;; 042:555555555555555555555555ddbbddbbddbbddbbddbbddbbddbbddbbddbbddbb
;; 043:555555555555555555555555ddbbddbbddbbddbbddbbddbbddbbddbbddbbddbb
;; 044:555555555555555555555555dd555555dd555555dd555555dd555555dd555555
;; 045:5555555555555555555555555555555555555555555555555555555555555555
;; 046:5555555555555555555555555555555555555555555555555555555555555555
;; 047:5555550055555500555555005555550055555500555555005555550055555500
;; 048:5555555555555555555555555555555555555555555555555555555555555555
;; 049:5555555555555555555555555555555555555555555555555555555555555555
;; 050:5555555555555555555555555555555555555555555555555555555555555555
;; 051:ccc55cc5ccc55cc5cccdcccdcddddccdccd55cc5ccddcccdccd6dccdcd6666c6
;; 052:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 053:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 054:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 055:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 056:5cc55cbb5cc55cbbcccdccbbdccddcaa5cc55caacccdccaadcc6dcaa6c6666aa
;; 057:ddbbddbbddbbddbbddbbddbbbbaabbaabbaabbaabbaabbaabbaabbaabbaabbaa
;; 058:ddbbddbbddbbddbbddbbddbbbbaabbaabbaabbaabbaabbaabbaabbaabbaabbaa
;; 059:ddbbddbbddbbddbbddbbddbbbbaabbaabbaabbaabbaabbaabbaabbaabbaabbaa
;; 060:ddc55cccddc55cccddddccccbbdddccdbbd55cccbbddc6ccbbdd66ccbb6666cd
;; 061:5555555555555555555555555555555566555555665555556555555565555555
;; 062:5555555555555555555555555555555555555555555555555555555555555555
;; 063:5555550055555500555555005555550055555500555555005555550055555500
;; 064:5555555555555555555555555555555555555555555555555555555555555555
;; 065:5555555555555555555555555555555555555555555555555555555555555555
;; 066:5555555555555555555555555555555555555555555555555555555555555555
;; 067:cccccccccccddddcccc9988ccdc9877cccc9878cccc9788cccc9778ccdc9878c
;; 068:6666666655665566555555556555555565555555655555556555555565555555
;; 069:6666666655665566555555555555555555555555555555555555555555555555
;; 070:6666666655665566555555555555555555555555555555555555555555555555
;; 071:6666666655665566555555555555555555555555555555555555555555555555
;; 072:6666666d5566556d5555555d5555555d5555555d5555555d5555555d5555555d
;; 073:dddddddddddccdddddccccdddccccccddccccccddccccccddccccccddccccccd
;; 074:dddddddddddccdddddccccdddccccccddccccccddccccccddccccccddccccccd
;; 075:dddddddddddccdddddccccdddccccccddccccccddccccccddccccccddccccccd
;; 076:d66666ccd66655ccd66555ccd66555cdd66555ccd66555ccd66555ccd66555cd
;; 077:6655555566555555655555556555555566555555665555556555555565555555
;; 078:5555555555555555555555555555555555555555555555555555555555555555
;; 079:5555550055555500555555005555550055555500555555005555550055555500
;; 080:5555555555555555555555555555555555555555555555555555555555555555
;; 081:5555555555555225555521125525622552125555552652555555212555556255
;; 082:5555555555555555555555555555555555555555555555555555555555555555
;; 083:ccc9877cccc9877cccc9778ccdc9788cccccccccccddddddccddddddcd666666
;; 084:6555555565555555655555556555555565555555655555556555555565555555
;; 085:5555555555555555555555555555555555555555555555555555555555555555
;; 086:5555555555555555555555555555555555555555555555555555555555555555
;; 087:5555555555555555552255555211255555225525555652125555562555555555
;; 088:5555555555555555555555555555555555555555555555555555555555555555
;; 089:cccccccc1ccccccc111111115111111151111111551111115555511155555555
;; 090:cccccccccccccccc111111111111111111111111111111111111111155511111
;; 091:cccccccccccccccc111111111111111511111111111111111111111511111155
;; 092:666555cc666555cc555555cc555555cd555555cc555555cc555555cc555555cd
;; 093:6655555566555555655555556555555566555555665555556555555565555555
;; 094:5555555555555555555555555555555555555555555555555555555555555555
;; 095:5555550055555500555555005555550055555500555555005555550055555500
;; 096:5555555555555555555555555555555555555555555555555555555555555555
;; 097:5555555555555555555555555555555555555555555555555555555555555555
;; 098:5555555555555555555555555555555555555555555555555555555555555555
;; 099:cccccccccccd3ddcccc3232ccdc3232cccc2232cccc2323cccc3232ccdc2232c
;; 100:5555555555555555555555556555555565555555655555556555555565555555
;; 101:555afff555ffffaf5afffffa00afafab000abab0600606665666666555555555
;; 102:5555555555555555555555555555555555555555555555555555555555555555
;; 103:5555555555555555555555555555555555555555555555555555555555555555
;; 104:5555555555555555555555555555555555555555555555555555555555555555
;; 105:5555555555555555555555555555555555555555555555555555555555555555
;; 106:555551115affaa11affff001fffff000afaffa00aabaabbc60bbb0cc566cccc1
;; 107:1111155511111555111115551111155511115555111155551111555511115555
;; 108:555555cc555555cc555555cc555555cd555555cc555555cc555555cc555555cd
;; 109:6655555566555555655555556555555566555555665555556555555565555555
;; 110:5555555555555555555555555555555555555555555555555555555555555555
;; 111:5555550055555500555555005555550055555500555555005555550055555500
;; 112:5555555555555555555555555555555555555555555555555555555555555555
;; 113:5555555555555555555555555555555555555555555555555555555555555555
;; 114:5555555555555555555555555555555555555555555555555555555555555555
;; 115:ccc3232cccc2322cccc2322ccdc2323cccccccccccddddddccddddddcd666666
;; 116:6555555565555555655555556555555565555555655555556555555565555555
;; 117:5555555555115555512215555511555555565155555512155555516555555555
;; 118:5555555555555555555555555555555555555555555555555555555555555555
;; 119:5555555555555555555555555555555555555555555555555555555555555555
;; 120:555555555fffa555faffff55afffffa5bafafa000baba0006660600656666665
;; 121:5555555555555555555555555555555555555555555555555555555555555555
;; 122:5551111155511111555111115551111155511111555111115551111155551111
;; 123:1111555511155555111555551115555511155555111155551111555511115555
;; 124:555555cc555554cc555554cc544455cd554444cc555445cc555555cc555555cd
;; 125:6655555566555555655555556555555566555555665555556555555565555555
;; 126:5555555555555555555555555555555555555555555555555555555555555555
;; 127:5555550055555500555555005555550055555500555555005555550055555500
;; 128:5555555555555555555555555555555555555555555555555555555555555555
;; 129:5555555555555555555555555555555555555555555555555555555555555555
;; 130:5555555555555555555555555555555555555555555555555555555555555555
;; 131:ccc65cc5ccc65cc5cccdcccdcccddccdccc55cc5cccdcccdccc6dccd6c6666c6
;; 132:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 133:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 134:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 135:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 136:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 137:5cc55cc55cc55cc5cccdcccddccddccd5cc55cc5cccdcccddcc6dccd6c6666c6
;; 138:5cccc1c15cccdcdcccccdddddccab1d15ccabcdcccccdddddc6cd1d166661111
;; 139:1c1cccc5cdcdccc5dddacccd1d1dcccdcdcdccc5dddacccd1d1dcccd1111c6c6
;; 140:5cc55ccc5cc55cccdcccdcccdccddccc5cc55cccdcccdcccdccd6ccc5c6666c6
;; 141:6655555566555555655555556555555566555555665555556555555565555555
;; 142:5555455555544555555455554455555554454555555544555555445555555455
;; 143:5555550055555500555555005555550055555500555555005555550055555500
;; 144:5555555555555555555555555555555555555555555555555555555555555555
;; 145:5555555555555555555555555555555555555555555555555555555555555555
;; 146:5555455555544555555455554455555554454555555544555555445555555455
;; 147:5666666655665566555555555555555555555555555555555555555555555555
;; 148:6666666655665566555555555555555555555555555555555555555555555555
;; 149:6666666655665566555555555555555555555555555555555555555555555555
;; 150:6666666655665566555555555555555555555555555555555555555555555555
;; 151:6666666655665566555555555555555555555555555555555555555555555555
;; 152:6666666655665566555555555555555555555555555555555555555555555555
;; 153:6666666655665566555555555545454455454545554545445545454545544544
;; 154:66666ccc55666ccc555551115555511145555511555555114555551155455511
;; 155:cccc6666cccc6666111115551111155511111555111115551111155511111555
;; 156:6666666655665566555555555555555555555555555555555555555555555555
;; 157:6655555566115555512215555511555555565155555512155555516555555555
;; 158:5555555555555555555555555555555555555555555555555555555555555555
;; 159:5555550055555500555555005555550055555500555555005555550055555500
;; 160:5555555555555555555555555555555500000000000000000000000000000000
;; 161:5555555555555555555555555555555500000000000000000000000000000000
;; 162:5555555555555555555555555555555500000000000000000000000000000000
;; 163:5555555555555555555555555555555500000000000000000000000000000000
;; 164:5555555555555555555555555555555500000000000000000000000000000000
;; 165:5555555555555555555555555555555500000000000000000000000000000000
;; 166:5555555555555555555555555555555500000000000000000000000000000000
;; 167:5555555555555555555555555555555500000000000000000000000000000000
;; 168:5555555555555555555555555555555500000000000000000000000000000000
;; 169:5555555555555555555555555555555500000000000000000000000000000000
;; 170:5555551155555111555551115555511100000000000000000000000000000000
;; 171:1111155511111555111155551111555500000000000000000000000000000000
;; 172:5555555555555555555555555555555500000000000000000000000000000000
;; 173:5555555555555555555555555555555500000000000000000000000000000000
;; 174:5555555555555555555555555555555500000000000000000000000000000000
;; 175:5555550055555500555555005555550000000000000000000000000000000000
;; </TILES>

;; <SPRITES>
;; 000:5555555555aaffa5500ffffa000fffff00affafa6bbaabaa660bbb0656666665
;; 001:5555555555aaffa5500ffffa000fffff00affafa6bbaabaa6660b06656666665
;; 002:55555555555afff555ffffaf5afffffa00afafab000abab06006066656666665
;; 003:55555555555afff555ffffaf00fffffa000fafab600abab05666066655566665
;; 016:5505055555000555c0808055c0000f0555fff00055000005556000065556f6f5
;; 017:5505055555000555c0808055c0000f0555fff0005500000555600006555f666f
;; 018:5505055555000555c0808055c0000f0555fff00055000005556000065556f6f5
;; 019:5505055555000555c0808055c0000f0555fff00555000000556000065556f6f5
;; </SPRITES>

;; <MAP>
;; 001:002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <FLAGS>
;; 000:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010101010101000000010000000000000100000000000000000100000000000001000000000000000001000000000000010000000000000000010000000000000100000000000000000100000000000001010101010101010101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </FLAGS>

;; <PALETTE>
;; 000:2e3740fbfcaaffce00f4b41bb6d53c71aa343f7e0092f4ff42cafd3978a8bcb7c58d87a2bf79587a444aff0000f8f8f8
;; </PALETTE>

