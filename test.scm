(require "lazybotz.scm")

(go-laziness)

(gravity (vector 0 -0.1 0))

(define (a) (* 45 (sin (* 10 (time)))))
(define (b) (* 45 (cos (* 30 (time)))))

(texture (load-texture "textures/0.png"))

(bot 'fred 
    (x 0 
        (x 4 b
            (x 2) 
            (x 2 (x 2)
                (x 0))) 
        (x 2 a 
            (x 0 a))) 'go)

(texture (load-texture "textures/1.png"))

(bot 'frank
    (x 0 
        (x 4 b
            (x 2)
            (x 0 (x 1 (x 0 (x 3))))
            (x 2 (x 2)
                (x 0))) 
        (x 2
            (x 2 (x 2 (x 2)))
            (x 0))) 'go)

(texture (load-texture "textures/2.png"))

(bot 'fredwina
    (x 0 
        (x 3 b
            (x 2) 
            (x 2)
            (x 2)) 
        (x 1 a
            (x 2 (x 4 a)) 
            (x 2 (x 5 a)))
        (x 3 b)
        (x 3 b)) 'go)

(texture (load-texture "textures/3.png"))

(bot 'phillip
    (x 1 (x 1) (x 1)))