
(clear)
(define p (with-state
    ;(scale (vector 1 0.3 0.3))
    (translate (vector 0.5 0 0))
    (build-cube)))

(with-primitive p
    (save-primitive "meshes/body-01.obj"))