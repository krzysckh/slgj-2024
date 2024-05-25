(define-library (puz map)
  (import
   (owl toplevel)
   (raylib)
   (puz util))

  (export
    doors-open
    additional-rand-blocks
    nono-blocks
    replace-blocks
    symthing?
    drawme?
    button?
    button-target?
    !button-target?
    normal-text?
    small-text?
    door?
    door-ending?
    finish?
    aq
    load-map
    find-thing
    find-things
   )

  (begin
    (define additional-rand-blocks '((5 8) (8 8)))

    (define doors-open
      '((4 0) (5 0) (7 0)
        (1 1) (2 1) (4 1) (5 1) (6 1) (7 1)
        (0 2) (1 2) (3 2) (4 2)
        (4 3) (5 3) (7 3) (8 3)
        (1 4) (2 4)))

    ;; block player cannot move through
    (define nono-blocks    (list #\= #\|))
    (define replace-blocks (list #\= #\space))

    (define (symthing? sym)
      (λ (thing)
        (if (symbol? (car* thing))
            (eqv? (car* thing) sym)
            #f)))

    (define drawme?         (symthing? 'drawme))
    (define button?         (symthing? 'btn))
    (define button-target?  (symthing? 'btn-target))
    (define !button-target? (symthing? '!btn-target))
    (define normal-text?    (symthing? '%-text))
    (define small-text?     (symthing? '^-text))
    (define (door? c) (and (>= c #\a) (<= c #\z)))
    (define (door-ending? c) (and (>= c #\A) (<= c #\Z)))
    (define (finish? c) (eqv? c #\$))

    (define (fix-length m ml)
      (append m (make-list (- ml (length m)) #\space)))

    (define rand-block-% 20)
    (define (add-random-blocks m)
      (letrec ((f (λ (r line acc)
                    (cond
                     ((null? line) (values r acc))
                     ((= (car line) #\=)
                      (lets ((R (/ rand-block-% 100))
                             (r v (rand-range r (- (numerator R) 1) (denominator R))))
                            (if (= v (numerator R))
                                (lets ((r v (rand-range r 0 (length additional-rand-blocks))))
                                      (f r (cdr line)
                                         (append acc (list (append '(drawme door) (lref additional-rand-blocks v))))))
                                (f r (cdr line) (append acc (list (car line)))))))
                     (else
                      (f r (cdr line) (append acc (list (car line)))))))))
        (let loop ((r (seed->rands (time-ms))) (m m) (acc ()))
          (if (null? m)
              acc
              (lets ((r l (f r (car m) ())))
                    (loop r (cdr m) (append acc (list l))))))))

    (define (floodfill-emptyness m pt)
      (cond
       ((< (car pt) 0) m)
       ((< (cdr pt) 0) m)
       ((>= (cdr pt) (length m)) m)
       ((>= (car pt) (length (lref m (cdr pt)))) m)
       ((list? (lref (lref m (cdr pt)) (car pt))) m)
       ((not (= (lref (lref m (cdr pt)) (car pt)) #\space)) m)
       (else
        (let* ((m (lset m (cdr pt) (lset (lref m (cdr pt)) (car pt) #\_)))
               (m (floodfill-emptyness m `(,(car pt) . ,(+ (cdr pt) 1))))
               (m (floodfill-emptyness m `(,(car pt) . ,(- (cdr pt) 1))))
               (m (floodfill-emptyness m `(,(- (car pt) 1) . ,(cdr pt))))
               (m (floodfill-emptyness m `(,(+ (car pt) 1) . ,(cdr pt)))))
          m))))

    (define (gettext c l acc)
      (if (eqv? c (car l))
          (values l (list->string acc))
          (gettext c (cdr l) (append acc `(,(car l))))))

    (define (find-multichar l)
      (let loop ((l l) (acc ()))
        (cond
         ((null? l) acc)
         ((list? (car l)) (loop (cdr l) (append acc (list (car l)))))
         ((= (car l) #\.) (loop (cddr l) (append acc (list `(btn ,(cadr l))))))
         ((= (car l) #\|) (loop (cddr l) (append acc (list `(btn-target ,(cadr l))))))
         ((= (car l) #\!) (loop (cddr l) (append acc (list `(!btn-target ,(cadr l))))))
         ((has? '(#\^ #\%) (car l))
          (lets ((ls s (gettext (car l) (cdr l) ())))
                (loop (cdr ls) (append acc (list `(,(string->symbol (string-append (string (car l)) "-text")) ,s))))))
         (else
          (loop (cdr l) (append acc (list (car l))))))))

    (define (load-map f)
      (let* ((m (map string->list (force-ll (lines (open-input-file f)))))
             (_ (print "load-map OK load file"))
             (m (map (λ (l) (append '(#\space) l '(#\space))) m))
             (ml (maxl (map length m)))
             (m (map (λ (x) (fix-length x ml)) m))
             (m (append (list (make-list ml #\space)) m (list (make-list ml #\space))))
             (_ (print "load-map OK append lines"))
             (m (add-random-blocks m))
             (_ (print "load-map OK add-random-blocks"))
             (m (map find-multichar m))
             (_ (print "load-map OK find-multichar"))
             (m (floodfill-emptyness m '(0 . 0)))
             (_ (print "load-map OK floodfill-emptyness")))
        m))

    (define (find-things c Map . lmax)
      (let ((f (if (function? c) c (λ (x) (eqv? x c))))
            (max (if (null? lmax) (<< 2 32) (car lmax))))
        (let loop ((x 0) (y 0) (acc ()))
          (cond
           ((>= (length acc) max) acc)
           ((>= y (length Map)) acc)
           ((>= x (length (lref Map y))) (loop 0 (+ y 1) acc))
           ((f (lref (lref Map y) x)) (loop (+ x 1) y (append acc (list (list x y)))))
           (else
            (loop (+ x 1) y acc))))))

    (define (find-thing c Map)
      (car (find-things c Map)))
    ))
