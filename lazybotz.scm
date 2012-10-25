#lang scheme/base
(require fluxus-018/fluxus)
(provide (all-defined-out))

(define (make-component . args)
    (define (list->type l)
        (cond
            ((null? l) 'end)
            ((eq? (length l) 1) 'link)
            (else 'body)))
    (let ((var (car args))
            (args (cdr args)))
        ; no args, then it's an end with no behaviour
        (if (null? args)
            (list (list->type args) #f var '())
            ; if the first item is not a list, then assume 
            ; it's the behaviour function
            (if (list? (car args))
                (list (list->type args) #f var args)
                (list (list->type (cdr args)) (car args) var (cdr args))))))

(define x make-component)

(define (component-type component)
    (list-ref component 0))

(define (component-behaviour component)
    (list-ref component 1))

(define (component-variation component)
    (list-ref component 2))

(define (component-children component)
    (list-ref component 3))

(define (component-print component)
    (define (_ component depth)
        (for ((i (in-range 0 (* 2 depth))))
            (printf " "))
        (printf "type: ~a / ~a~n" 
            (component-type component)
            (if (component-behaviour component)
                "has behaviour"
                "no behaviour"))
        (for-each
            (lambda (child)
                (_ child (+ depth 1)))
            (component-children component)))
    (_ component 0))

(define (safe-list-ref l i)
    (list-ref l (modulo i (length l))))

(define (component-build component parent-obj pos norm axis tx object-list behaviours physical)
    (let* ((object (safe-list-ref (object-list/type object-list (component-type component)) 
                    (component-variation component)))
            (p (with-state        
                    (concat tx)            
                    (translate pos)
                    (when (not (and (eq? (vx norm) 1)
                                (eq? (vy norm) 0)
                                (eq? (vz norm) 0)))
                        (concat (maim norm (vector 1 0 0))))
                    (hint-cast-shadow)
                    (load-primitive (object-objfile object))))
            (connections (object-connections object))
            (index -1)
            (hinge-pos (vtransform pos tx))
            (hinge-axis (vtransform-rot axis tx))
            (tx (with-primitive p (get-transform))))
        
        (with-primitive p (apply-transform) (recalc-normals 0))
        (when physical (active-box p))
        
        (let ((behaviour
                    (if (and physical (not (zero? parent-obj)))
                        (with-primitive p
                            (let ((joint (build-hingejoint p parent-obj hinge-pos hinge-axis)))
                                (if (component-behaviour component)
                                    (list joint (component-behaviour component))
                                    #f)))
                        #f)))
            
            
            (foldl
                (lambda (child r)
                    (cond ((< (+ index 1) (length connections))
                            (set! index (+ index 1))
                            (let ((b (component-build child p
                                            (connection-pos (list-ref connections index))
                                            (connection-norm (list-ref connections index)) 
                                            (connection-axis (list-ref connections index))
                                            tx object-list behaviours physical)))
                                (if (null? b) r (append b r))))
                        (else r)))
                (if behaviour (list behaviour) '())
                (component-children component)))))

;---

(define (make-object connections objfile)
    (list connections objfile))

(define (object-connections object)
    (list-ref object 0))

(define (object-objfile object)
    (list-ref object 1))

(define (connection-pos connection)
    (list-ref connection 0))

(define (connection-norm connection)
    (list-ref connection 1))

(define (connection-axis connection)
    (list-ref connection 2))

;---
; data driven-o-rama

(define (make-object-list bodies links ends)
    (list bodies links ends))

(define (load-object-list comp-path mesh-path)
    (let ((raw-list
                (map
                    (lambda (file)            
                        (if (not (char=? (string-ref (path->string file) 0) #\.))
                            (let* ((file (path->string file))
                                    (f (open-input-file (string-append comp-path file)))
                                    (connections (read f)))
                                (close-input-port f)
                                (list connections 
                                    (string-append mesh-path 
                                        (substring file 0 (- (string-length file) 2))
                                        "obj")))
                            (list '() "")))
                    (directory-list comp-path))))
        (make-object-list
            (filter
                (lambda (obj)
                    (and (not (string=? (object-objfile obj) "")) 
                        (not (eq? (length (object-connections obj)) 1))
                        (not (null? (object-connections obj)))))
                raw-list)
            (filter
                (lambda (obj)
					(and (not (string=? (object-objfile obj) "")) 
	                    (eq? (length (object-connections obj)) 1)))
                raw-list)
            (filter
                (lambda (obj)
					(and (not (string=? (object-objfile obj) "")) 
	                     (null? (object-connections obj))))
                raw-list))))

(define (object-list/type object-list type)
    (cond
        ((eq? type 'body) (object-list-bodies object-list))
        ((eq? type 'link) (object-list-links object-list))
        ((eq? type 'end) (object-list-ends object-list))))

(define (object-list-bodies object-list)
    (list-ref object-list 0))

(define (object-list-links object-list)
    (list-ref object-list 1))

(define (object-list-ends object-list)
    (list-ref object-list 2))                          

;--

(define (make-bot root-component tex object-list physical)
    (let ((root (build-locator)))
        (with-state
            (parent root)
            (list root-component tex 
                (component-build root-component 0 
                    (vector 0 0 0) (vector 1 0 0) (vector 0 0 1) (mident) object-list '()
                    physical)
                root physical))))

(define (bot-root-component bot)
    (list-ref bot 0))

(define (bot-tex bot)
    (list-ref bot 1))

(define (bot-behaviours bot)
    (list-ref bot 2))

(define (bot-root bot)
    (list-ref bot 3))

(define (bot-physical bot)
    (list-ref bot 4))

(define (bot-update bot)
    (for-each
        (lambda (b)
            (joint-angle (car b) 10 (/ ((cadr b)) 57.2958)))
        (bot-behaviours bot)))

(define (bot-destroy bot)    
    (destroy (bot-root bot)))

;--

(define (make-world bots)
    (list bots))

(define (world-bots world)
    (list-ref world 0))

(define (world-add-bot world name bot)
    (define (_ bots out)
        (cond
            ((null? bots) (cons (list name bot) out))
            ((eq? (car (car bots)) name)
                (when (not (bot-physical (cadr (car bots)))) 
                    (bot-destroy (cadr (car bots))))
                (append (cons (list name bot) out) (cdr bots)))
            (else
                (_ (cdr bots) (cons (car bots) out)))))
    (make-world (_ (world-bots world) '())))

(define (world-remove-bot world name)
    (define (_ bots out)
        (cond
            ((null? bots) out)
            ((eq? (car (car bots)) name)
                (_ (cdr bots) out))
            (else
                (_ (cdr bots) (cons (car bots) out)))))
    (make-world (_ (world-bots world) '())))

(define (world-print world)
    (for-each
        (lambda (bot)
            (printf "~a~n" (car bot))
            (component-print (bot-root-component (cadr bot))))
        (world-bots world)))

(define (world-update world)
    (for-each
        (lambda (bot)
            (bot-update (cadr bot)))
        (world-bots world)))

;--

(define w 0)
(define object-list 0)

(define (go-laziness)
    (clear)
    (gravity (vector 0 0 0))
    (collisions 1)
    (ground-plane (vector 0 1 0) -5)
    
	(let ((size 5))
	(ground-plane (vector 0  1 0) (- size))
    (ground-plane (vector -1 0 0) (- size))
    (ground-plane (vector 1  0 0) (- size))    
    (ground-plane (vector 0  0 1) (- size))
    (ground-plane (vector 0 0 -1) (- size)))
	
    (set-physics-debug #f)
    
    (with-state
        (rotate (vector -90 0 0))
        (translate (vector 0 0 -5))
        (specular (vector 0 0 0))
        (scale 100)
        (colour 1)
        (build-seg-plane 20 20))
    
    (light-diffuse 0 (vector 0.4 0.4 0.4))
    (light-specular 0 (vector 0.1 0.1 0.1))
    
    (let ((mylight (make-light 'spot 'free)))
        (light-position mylight (vector 20 50 -20))
        (light-diffuse mylight (vector 1 1 1))
        (light-spot-angle mylight 20)
        (light-spot-exponent mylight 100)
        (light-attenuation mylight 'constant 1) 
        (light-direction mylight (vector -0.2 -0.8 0.3))
        (shadow-light mylight))
    
    (show-axis 0)
    
    (set! w (make-world '()))
    (set! object-list (load-object-list "comps/" "meshes/"))
    (every-frame (world-update w)))

(define (bot name desc (p #f))
    (set! w (world-add-bot w name (make-bot desc "" object-list (eq? p 'go)))))

(every-frame (world-update w))

(go-laziness)
