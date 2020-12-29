;; title:  Sheepdog
;; author: Stephen Wassell
;; desc:   short description
;; script: fennel

; TIC-80 screen size.
(local screen-w 240)
(local screen-h 136)

; Background of the map and transparent sprites - dark green.
(local bg-colour 5)

; Incremented on each frame.
(var t 0)

; Tables of entity id => closure.
; These are called on each frame in this order.
(var updaters {}) ; id (fn [])
(var drawers {}) ; id { :z z :f (fn []) }
; Call to check if the provides coords collide with each entity in the table.
(var collides {}) ; id (fn [x y w h])

(global TIC
  (fn tic []
    "The main game loop."
    (each [_ f (pairs updaters)] (f))
    ; todo: will this work? needs to be ipairs?
    (table.sort drawers #(< $1.z $2.z))
    (each [_ i (pairs drawers)] (i.f))
    (set t (+ t 1))))

(fn xy-to-x [x y]
  "Calculate z-index from x and y coords. todo: is 0 fg or bg?"
  ((+ (* y 512) x)))

(fn any-collides [x y w h check-id prev-id]
  "Return true if the provided coords collide with any other entity."
  ; Get the first/next collide closure. The prev-id param is initially nil.
  (local iter-id (next collides prev-id))
  (if
    (= iter-id nil) false ; Reached end of list, no collisions.
    (= iter-id check-id) (any-collides x y w h check-id iter-id) ; Don't collide with itself.
    ((. collides iter-id) x y w h) true ; A collide closure returned true.
    (any-collides x y w h check-id iter-id))) ; Tail recursion

; Define function for constructors to call to get a new unique id.
; It's closure returned by new-unique-id.
(local unique-id ((fn []
  "Return a closure which increments id and returns the new value."
  (var id 0)
  (fn []
    (set id (+ 1 id))
    id))))

(fn new-map []
  "Create a new map object: draw the background, check for screen edge collisions."
  (local id (unique-id))

  (tset drawers id { :z 0 :f (fn []
    (cls bg-colour)) } )

  (tset collides id (fn [test-x test-y test-w test-h]
    (or
      (< test-x 0)
      (< test-y 0)
      (> (+ test-x test-w -1) screen-w)
      (> (+ test-y test-h -1) screen-h)))))

(fn normalise [x y mag]
  "Normalise vector to a given magnitude."
  (local scale (math.sqrt (+ (* x x) (* y y))))
  (if
    (< scale 1) (values 0 0) ; Avoid divide by 0 and excessive wiggling.
    (values (* (/ x scale) mag) (* (/ y scale) mag))))

(fn buttons []
  "Array of 0/1 for buttons pressed: up down left right"
  [
    (if (btn 0) 1 0)
    (if (btn 1) 1 0)
    (if (btn 2) 1 0)
    (if (btn 3) 1 0)
  ])

(fn to-mouse [from-x from-y]
  "Return vector from x y to mouse, if any buttons pressed (else 0 0)."
  (local (mouse-x mouse-y left middle right scrollx scrolly) (mouse))
  (if
    (or left middle right) (values (- mouse-x from-x) (- mouse-y from-y))
    (values 0 0)))

(fn get-action [from-x from-y accel]
  "Return direction to move player based on buttons and mouse."
  ; up down left right
  (local (ax ay) (match (buttons)
    [1 0 0 0] (values 0 -1)
    [0 1 0 0] (values 0 1)
    [0 0 1 0] (values -1 0)
    [0 0 0 1] (values 1 0)
    [1 0 1 0] (values -1 -1)
    [1 0 0 1] (values 1 -1)
    [0 1 1 0] (values -1 1)
    [0 1 0 1] (values 1 1)
    _ (to-mouse from-x from-y))) ; No buttons pressed, go towards mouse.
  (normalise ax ay accel))

(fn center [x w]
  "The center of a sprite given x and width (or y and height)."
  (+ x (/ w 2)))

(fn alternate [s]
  "Alternate between s and s+1 for running feet and wagging tails."
  (+ s (/ (% t 20) 10)))

(fn moving [dx dy]
  "True if we need the sprite for moving, given pixels per frame."
  (or (> (math.abs dx) .1) (> (math.abs dy) .1)))

(fn new-player []
  "Create a new player object: respond to keys, draw on the foreground."
  (local id (unique-id))
  
  ; Size of the dog. todo: shorter than this
  (local w 8)
  (local h 8)

  ; Current location, updated in drawer.
  (var x (/ (- screen-w w) 2))
  (var y (/ (- screen-h h) 2))

  ; Current velocity in pixels per frame, updated in updater.
  (var dx 0)
  (var dy 0)

  ; How fast can it run.
  (local accel .15)
  (local friction .9)

  ; Sprite indices, also +1 to each for alternate.
  (local spr-run 272)
  (local spr-idle 274)

  (tset updaters id (fn []
    "Update dx dy based on buttons and mouse."
    (local (ax ay) (get-action (center x w) (center y h) accel))
    (set dx (* friction (+ dx ax)))
    (set dy (* friction (+ dy ay)))
    (when (any-collides (+ x dx) (+ y dy) w h id) (set dx 0) (set dy 0))))

  (tset drawers id { :z 1 :f (fn []
    "Update x y from dx dy and draw the sprite there."
    (set x (+ x dx))
    (set y (+ y dy))
    (local s (if (moving dx dy) spr-run spr-idle))
    (local flip (if (> dx 0) 1 0)) ; Images face left.
    (spr (alternate s) x y bg-colour 1 flip)) } ))

; Create the persistent entities.
(new-map)
(new-player)

;; <TILES>
;; 001:efffffffff222222f8888888f8222222f8fffffff8ff0ffff8ff0ffff8ff0fff
;; 002:fffffeee2222ffee88880fee22280feefff80fff0ff80f0f0ff80f0f0ff80f0f
;; 003:efffffffff222222f8888888f8222222f8fffffff8fffffff8ff0ffff8ff0fff
;; 004:fffffeee2222ffee88880fee22280feefff80ffffff80f0f0ff80f0f0ff80f0f
;; 017:f8fffffff8888888f888f888f8888ffff8888888f2222222ff000fffefffffef
;; 018:fff800ff88880ffef8880fee88880fee88880fee2222ffee000ffeeeffffeeee
;; 019:f8fffffff8888888f888f888f8888ffff8888888f2222222ff000fffefffffef
;; 020:fff800ff88880ffef8880fee88880fee88880fee2222ffee000ffeeeffffeeee
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
;; 000:00000000000000ffff00000000ffffffff0123456789ab00effe00ba987600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c22d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500002000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013
;; 003:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050
;; 004:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ea
;; 008:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000095
;; 009:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003383000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000027
;; 010:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e23c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a5
;; 011:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007783000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000057
;; 012:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000344700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008300c100a80100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000095
;; 013:00000000000000000000000000000000000000000000000000000000000005b50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ba8300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002c0000000047000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c4
;; 014:00000000000000000000000000000000000000000000000000000000000056c60000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008300b2009808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b5
;; 015:0000000000000000000000000000000000000000000000000000000000000be5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ab0a6074011600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:000000000000000000000000000000000000000000000000000000000000f5f500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 018:0000000000000000000000000000000000000000000000000000000000008d130000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000044000000a600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 019:000000000000000000000000000000000000000000000000000000000000099500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 020:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 021:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000044000000e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 022:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000085c764018100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 030:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006fcf008f00000000000000000000000000000000000000000000016400
;; 031:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008100000000000000000000000000000000000000000000010000
;; 032:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000022953500440000000000000083c37401000000006400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008600000000000000000000000000000000000000000000016400
;; 033:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007bfcf00859f64012295350044000000000000000c4d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008100000000000000000000000000000000000000000000004400
;; 034:00000000000000000000000000000000000000000000000000000000000044000000000000000caa7401086554016400000007bfcf000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008a1c64018f5274016400000007bfcf000c9b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002c6963700000000000000000000000000000000000000000000471626
;; 035:c600000000000000000000000000000000000000000000000000000000000202020266f62702b6c2020716470296e602071696273700000000000000000000000000000000000000000000000000000000000000000000000000000000000000020202020296660282f5f566e6c6f576c6f62616c6f5f5370000000000000000000000000000000000000000000000000000000096e6768207164792929202478656e6a002020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000004602275600000000000000000000000000000000000000000000f63796
;; 036:4700000000000000000000000000000000000000000000000000000000002696e64696e67637c202071647475627e6b582b602b2020000000000000000000000000000000000000000000000000000000000e637562747822696e64696e67637c202b7c6963747823797d682723756c65636477292c202b60000000000000000000000000000000000000000000000000000000057e6071636b67292c2023797d68272471626c656e257e6071636b6729292c2026716c692000000000000000000000000000000000000000000000000000000272202d3000000000000000000000000000000000000000000000247f6
;; 037:370000000000000000000000000000000000000000000000000000000000e6a0020202020202020202022756475727e602e696c6a0020202020202020256c63756a002020202020202020202c6f63616c6023757266716c602d302c6963747823797d68272e27292c2026716c6c202b692a00202020202020000000000000000000000000000000000000000000000000000000096e64696e6763702d302d616473686f5071647475627e682b73757266716c6d7c2020700000000000000000000000000000000000000000000000000000002471626c6000000000000000000000000000000000000000000002636f6
;; 038:e6000000000000000000000000000000000000000000000000000000000002960716962737823757262696e64696e67637920246f6a0020202020202020202020202471626c656e296e637562747822696e64696e67637c2022692000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0020202020202f573f58292a00202020256e646a0020202022756475727e60236f6e6000000000000000000000000000000000000000000000000000000005000880000000000000000000000000000000000000000000000000000
;; 039:000000000000000000000000000000000000000000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000008f73740171c63500640000008d6364018953640122000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007bfcf008300000000000000000000000000000000000000000000000000
;; 040:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004b7401830000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 041:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006401894374000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 043:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 045:050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003ef7000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 046:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001fffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 047:e40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 050:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000440000008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <PALETTE>
;; 000:140c1c44243430346d4e4a4e854c30346524d04648757161597dced27d2c8595a16daa2cd2aa996dc2cadad45edeeed6
;; </PALETTE>

