#lang racket

; True if node has no children
(define (leaf? node)
    (null? (children node)))

; Gets the name of the node
(define (datum node)
    (car node))

; Gets the children of the node
(define (children node)
    (cdr node))

; Gets the number of leaves in the tree
(define (count-leaves tree)
    (if (leaf? tree)
        1
        (count-leaves-in-forest (children tree))))

; Gets the number of leaves in a forest, works recursively with previous
(define (count-leaves-in-forest forest)
    (if (null? forest)
        0
        (+ (count-leaves (car forest))
            (count-leaves-in-forest (cdr forest)))))

; Creates a node with children
(define (make-node datum children)
    (cons datum children))

; Creates a node with no children
(define (leaf datum)
    (make-node datum '()))

; Makes each city in list a leaf
(define (cities name-list)
    (map leaf name-list))

; True if place is in tree
(define (in-tree? place tree)
    (or (equal? place (datum tree))
        (in-forest? place (children tree))))

; True if place is in forest, works recursively with previous
(define (in-forest? place forest)
    (if (null? forest)
        #f
        (or (in-tree? place (car forest))
            (in-forest? place (cdr forest)))))

; Finds full path of a city in a tree
(define (locate city tree)
    (if (equal? city (datum tree))
        (list city)
        (let ((subpath (locate-in-forest city (children tree))))
            (if subpath
                (cons (datum tree) subpath)
                #f))))

; Recursive helper for previous
(define (locate-in-forest city forest)
    (if (null? forest)
        #f
        (or (locate city (car forest))
            (locate-in-forest city (cdr forest)))))

; Counts nodes
(define (count-nodes tree)
    (if (leaf? tree)
        1
        (+ 1 (count-nodes-children (children tree)))))

; Recursive helper for previous
(define (count-nodes-children c)
    (if (null? c)
        0
        (+ (count-nodes (car c))
            (count-nodes-children (cdr c)))))

; Gets first child tree of a tree
(define (first-child-tree country tree)
    (cond
        [(null? (cdr tree)) '()]
        [(eq? country (caadr tree)) (car (children (cadr tree)))]
        [else (first-child-tree country (cdr tree))]))

; Gets height
(define (height-tree tree)
    (cond
        [(leaf? tree) 0]
        [(null? (cddr tree)) (+ 1 (height-tree (cadr tree)))]
        [else (+ 1 (max (height-tree (cadr tree)) (height-tree (caddr tree))))]))

; Building of custom world tree
(define world-tree
    (make-node
    'world
    (list
        (make-node
            'italy
            (cities '(venezia riomaggiore firenze roma)))
        (make-node
            '(united states)
            (list
                (make-node
                    'california
                    (cities '(berkeley (san francisco) gilroy)))
                (make-node
                    'massachusetts
                    (cities '(cambridge amherst sudbury)))
                (make-node
                    'ohio
                    (cities '(kent)))))
        (make-node
            'zimbabwe
            (cities '(harare hwange)))
        (make-node
            'china
	        (cities '(beijing shanghai guangzhou suzhou)))
        (make-node
            '(great britain)
            (list
                (make-node
                    'england
                    (cities '(liverpool)))
                (make-node
                    'scotland
                    (cities '(edinburgh glasgow (gretna green))))
                (make-node
                    'wales
                    (cities '(abergavenny)))))
        (make-node
            'australia
                (list
                    (make-node
                        'victoria
                        (cities '(melbourne)))
                    (make-node
                        '(new south wales)
                        (cities '(sydney)))
                    (make-node
                        'queensland
                        (cities '(cairns (port douglas))))))
        (make-node
            'honduras
            (cities '(tegucigalpa))))))


(define (main)
    (newline)
    (display "Racket tree building and traversal demo")
    (newline)
    (newline)
    (display "How tall is the tree?: ")
    (display (height-tree world-tree))
    (newline)
    (display "How many nodes?: ")
    (display (count-nodes world-tree))
    (newline)
    (display "How many leaves?: ")
    (display (count-leaves world-tree))
    (newline)
    (display "Is Beijing in the tree?: ")
    (display (in-tree? 'beijing world-tree))
    (newline)
    (display "Is Phoenix in the tree?: ")
    (display (in-tree? 'phoenix world-tree))
    (newline)
    (display "Where is Cairns?: ")
    (display (locate 'cairns world-tree))
    (newline)
    (newline))

(main)
