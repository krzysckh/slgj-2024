(import
 (owl toplevel)
 (owl lazy)
 (raylib))


;; (define (flatten x)
;;   (cond
;;    ((null? x) '())
;;    ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
;;    (else
;;     (list x))))

(define Map (map string->list (force-ll (lines (open-input-file "map.text")))))

(define initial-player-pos
  (let loop ((x 0) (y 0))
    (if (eqv? #\@ (list-ref (list-ref Map y) x))
        (cons x y)
        (let* ((x (modulo (+ x 1) (length (list-ref Map y))))
               (y (if (= (modulo x (length (list-ref Map y))) 0) (+ y 1) y)))
          (loop x y)))))

(define font-f (list->bytevector (file->list "proggy-square.ttf")))

(define width 640)
(define height width)
(define n-blocks 10)
(define block-size (/ width n-blocks))

(define dgl:min (- 0 (* 10 width)))
(define dgl:max (* 20 width))
(define (draw-grid-lines)
  (for-each
   (λ (v)
     (draw-line-simple dgl:min v dgl:max v white)
     (draw-line-simple v dgl:min v dgl:max white))
   (iota dgl:min block-size dgl:max))) ;; assuming width == height

(define (draw-thing thing rect)
  (cond
   ((= thing #\x) (draw-rectangle rect blue))
   (else
    0)))

(define (draw-map)
  (for-each
   (λ (n)
     (let ((line (list-ref Map n))
           (y (* block-size n)))
       (for-each
        (λ (v) (draw-thing (list-ref line v) (list (* block-size v) y block-size block-size)))
        (iota 0 1 (length line)))))
   (iota 0 1 (length Map))))

(define (real-v v)
  (* v block-size))

(define (draw-player pos)
  (draw-rectangle-rounded
   (list (real-v (car pos)) (real-v (cdr pos)) block-size block-size)
   0.6 10 red))

(define (camera ppos zoom)
  (list
   (list (- (/ width 2)  (* zoom (/ block-size 2)))
         (- (/ height 2) (* zoom (/ block-size 2))))
   (list (real-v (car ppos)) (real-v (cdr ppos)))
   0
   zoom))

;; TODO: animacje
;; TODO: particle w wątkach?
;; TODO: nie przenikaj przez ścianę

(define (main _)
  (set-target-fps! 60)
  (with-window
   width height "λ-test"
   (let ((font (list->font (bytevector->list font-f) ".ttf" 64 1024)))
     (let loop ((zoom 1) (ppos initial-player-pos) (grid #t))
       (let* ((ppos (if (key-pressed? key-a) (cons (- (car ppos) 1) (cdr ppos)) ppos))
              (ppos (if (key-pressed? key-d) (cons (+ (car ppos) 1) (cdr ppos)) ppos))
              (ppos (if (key-pressed? key-s) (cons (car ppos) (+ (cdr ppos) 1)) ppos))
              (ppos (if (key-pressed? key-w) (cons (car ppos) (- (cdr ppos) 1)) ppos))
              (grid (if (key-pressed? key-g) (not grid) grid))
              (zoom (min 2 (max 0.2 (+ zoom (* 4 (frame-time) (mouse-wheel)))))))
         (draw
          (clear-background black)
          (with-camera2d
           (camera ppos zoom)
           (begin ;; TODO: ugly hack - fix with-camera2d macro
             (when grid (draw-grid-lines))
             (draw-map)
             (draw-player ppos)
             (draw-text font "helo" '(0 0) 64 0 white)))
          (draw-fps '(0 0)))

         (if (window-should-close?)
             0
             (loop zoom ppos grid)))))))

main
