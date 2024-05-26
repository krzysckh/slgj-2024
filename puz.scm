;; -*- mode: scheme; compile-command: "ol-rl -r puz.scm" -*-

(import
 (owl toplevel)
 (owl lazy)
 (owl random)
 (raylib))

(import
 (puz assets)
 (puz util)
 (puz map)
 (puz move)
 (puz draw)
 (puz map-cache)
 (puz maze)
 )

(define n-mazes 3)

;; (create-maze 16 16 128 #\2)
;; (create-maze 32 32 999 #\3)

(define-syntax at-runtime
  (syntax-rules ()
    ((at-runtime mf)
     (λ ()
       (set-target-fps! (<< 2 32))
       (let ((v (save-cache (load-map-from-memory mf) #f)))
         (set-target-fps! target-fps)
         v)))))

(define Maps-init
  (list
   (load-map-maybe-cache "map.text")
   (at-runtime (create-maze 8 8 (time-ms) #\1))
   (at-runtime (create-maze 32 16 (time-ms) #\2))
   ))

;; TODO: particle w wątkach?
;; TODO: R - restart

(define (puz sounds textures finish)
  (let ((finish-f (λ () (finish textures sounds)))) ;; wow
    (let loop ((ppos (aq 'initial-player-pos (car Maps-init)))
               (camera-pos (real-p (aq 'initial-player-pos (car Maps-init))))
               (key-queue ())
               (blocksq (list (aq 'initial-blocks (car Maps-init))))
               (buttonsq (list (aq 'initial-button-states (car Maps-init))))
               (undo ())
               (mapq `(0))
               (Maps Maps-init)
               (debug #f))
      (lets ((ppos-prev ppos)
             (key-queue (append key-queue (current-keys)))
             (blocks (car blocksq))
             (buttons (car buttonsq))
             (ppos (maybe-door (aq 'doors (lref Maps (car mapq))) ppos sounds))
             (Maps ppos mapq blocksq buttonsq (maybe-change-map Maps ppos mapq blocksq buttonsq))
             (ppos key-queue blocks buttons
                   (dispatch-move (aq 'map (lref Maps (car mapq))) ppos key-queue (car blocksq) (car buttonsq) sounds finish-f))
             (debug (if (key-pressed? key-g) (not debug) debug))
             (camera camera-pos (camera ppos camera-pos))
             ;; maybe do undo?
             (ppos blocks mapq undo (if (key-pressed? key-u)
                                        (let ((lu (lref undo (max 0 (- (length undo) 2)))))
                                          (play-sound (aq 'undo sounds))
                                          (values (car lu) (cadr lu) (caddr lu) (ldel undo (- (length undo) 1))))
                                        (values ppos blocks mapq undo)))
             (blocksq (lset blocksq 0 blocks))
             (buttonsq (lset buttonsq 0 buttons)))
            (draw
             (clear-background black)
             (draw-background-textures textures)

             (with-camera2d
              camera
              (begin ;; TODO: ugly hack - fix with-camera2d macro
                (when debug (draw-grid-lines))
                (draw-map (aq 'map (lref Maps (car mapq))) ppos textures buttons)
                (draw-blocks blocks textures)
                (draw-player ppos textures)))

             (when debug
               (draw-text (aq 'font textures) (str* buttons) '(0 0) 32 0 white))

             ;; the shadow thingy
             (draw-rectangle
              `(0 0 ,width ,height)
              (color 0 0 0 (clamp 0 255 (- (floor (/ (vec2dist (real-p ppos) camera-pos) 4)) 25))))
             (draw-fps '(0 0)))

            (let ((undo (if (equal? (last undo ()) (list ppos blocks mapq))
                            undo
                            (append undo (list (list ppos blocks mapq))))))
              (if (window-should-close?)
                  0
                  (loop
                   ppos
                   camera-pos
                   key-queue
                   blocksq
                   buttonsq
                   undo
                   mapq
                   Maps
                   debug)))))))

(define (main-menu sounds textures finish)
  (lets ((font (aq 'font textures))
         (_ (print "font: " font))
         (title "[puz]")
         (title-w title-h (measure-text font title 42 0))
         (start-btn-t "Start game")
         (sb-w sb-h (measure-text font start-btn-t 32 0)))
    (print "in main-menu")
    (with-mainloop
     (let* ((md (mouse-delta))
            (∆sum (+ (car md) (cdr md)))
            (start-rect (list (- (/ width 2) (/ sb-w 2) 16)
                              (- (/ height 2) 16)
                              (+ sb-w 32)
                              (+ sb-h 32)))
            (⍺ (if (collision-point-rect? (mouse-pos) start-rect) 255 (floor (+ 128 (* 0.25 (abs ∆sum))))))
            (s-color (if (and (mouse-btn-down? mouse-button-left) (collision-point-rect? (mouse-pos) start-rect))
                         orange
                         (color 158 98 15 ⍺))))
       (draw
        (draw-background-textures textures)
        (draw-text font title (list (- (/ width 2) (/ title-w 2)) (/ height 4)) 42 0 white)
        (draw-rectangle-rounded start-rect 0.3 10 s-color)
        (draw-text font start-btn-t (list (- (/ width 2) (/ sb-w 2)) (/ height 2)) 32 0 white)
        )

       (when (and (mouse-btn-released? mouse-button-left) (collision-point-rect? (mouse-pos) start-rect))
         (play-sound (aq 'door sounds))
         (puz sounds textures finish))

       ))))


(define (finish textures sounds)
  (lets ((font (aq 'font textures))
         (title "Congrats! that's that")
         (title-w title-h (measure-text font title 42 0))
         (mm-btn-t "back to main menu")
         (mm-w mm-h (measure-text font mm-btn-t 32 0))
         (exit-btn-t "exit")
         (exit-w exit-h (measure-text font exit-btn-t 32 0))) ;
        (with-mainloop
         (let* ((md (mouse-delta))
                (∆sum (+ (car md) (cdr md)))
                (mm-rect (list (- (/ width 2) (/ mm-w 2) 16)
                                  (- (/ height 2) 16)
                                  (+ mm-w 32)
                                  (+ mm-h 32)))
                (exit-rect (list (- (/ width 2) (/ exit-w 2) 16)
                                 (- height (/ height 4) 16)
                                 (+ exit-w 32)
                                 (+ exit-h 32)))
                (mm-⍺ (if (collision-point-rect? (mouse-pos) mm-rect) 255 (floor (+ 128 (* 0.25 (abs ∆sum))))))
                (exit-⍺ (if (collision-point-rect? (mouse-pos) exit-rect) 255 (floor (+ 128 (* 0.25 (abs ∆sum))))))
                (mm-color (if (and (mouse-btn-down? mouse-button-left) (collision-point-rect? (mouse-pos) mm-rect))
                                 orange
                                 (color 158 98 15 mm-⍺)))
                (exit-color (if (and (mouse-btn-down? mouse-button-left) (collision-point-rect? (mouse-pos) exit-rect))
                                 orange
                                 (color 158 98 15 exit-⍺))))
           (draw
            (draw-background-textures textures)
            (draw-text font title (list (- (/ width 2) (/ title-w 2)) (/ height 4)) 42 0 white)
            (draw-rectangle-rounded mm-rect 0.3 10 mm-color)
            (draw-text font mm-btn-t (list (- (/ width 2) (/ mm-w 2)) (/ height 2)) 32 0 white)
            (draw-rectangle-rounded exit-rect 0.3 10 exit-color)
            (draw-text font exit-btn-t (list (- (/ width 2) (/ exit-w 2)) (- height (/ height 4))) 32 0 white) ;
            )

           (when (and (mouse-btn-released? mouse-button-left) (collision-point-rect? (mouse-pos) mm-rect))
             (play-sound (aq 'door sounds))
             (main-menu sounds textures finish))

           (when (and (mouse-btn-released? mouse-button-left) (collision-point-rect? (mouse-pos) exit-rect))
             (play-sound (aq 'door sounds))
             (exit-owl 0))

           ))))

(define (main _)
  (set-target-fps! target-fps)
  (with-window
   width height "puz"
   (let* ((_ (init-audio-device))
          (sounds (load-sounds))
          (textures (load-textures)))
     (for-each (λ (t) (set-texture-filter! (cdr t) texture-filter-bilinear)) textures)
     (main-menu sounds textures finish))))

main
