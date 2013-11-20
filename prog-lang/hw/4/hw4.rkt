
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; put your code below

;; (1) Produces a list of 'low' to 'high' at a resolution of 'stride'
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; (2) Use the map function to append a suffix to each element of the xs list
(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

;; (3) Returns the ith element of the list xs, where i is the remainder of n/length
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: negative number")
          (car (list-tail xs (remainder n (length xs)))))))

;; (4) Returns a list for a given stream 's' for n-steps (where n >= 0)
(define (stream-for-n-steps s n)
  (let ([stream (s)])
    (if (= n 0)
        null
        (cons (car stream) (stream-for-n-steps (cdr stream) (- n 1))))))

;; (5) Returns a stream like the natural number stream, but for every ith element divisible by 5, it's negated
(define funny-number-stream
  (letrec ([f (lambda (x) (cons 
                           (if (= (modulo x 5) 0)
                               (- x)
                               x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; (6) Returns a stream which produces dan.jpg and dog.jpg alternating
(define (dan-then-dog)
  (letrec 
      ((dan (lambda () (cons "dan.jpg" dog)))
       (dog (lambda () (cons "dog.jpg" dan))))
    (dan)))

;; (7) Returns a stream where for each element v, the pair (0 . v) is returned
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) 
                   (lambda () ((stream-add-zero (cdr (s))))))))

;; (8) Returns a stream which cycles through two lists, xs and ys
(define (cycle-lists xs ys)
  (letrec ([thunk 
            (lambda (n)
              (cons                               ;; cons cell for stream eval
               (cons (list-nth-mod xs n)          ;; create nth mod entry of xs' as #1
                     (list-nth-mod ys n))         ;; create nth mod entry of ys' as #2
               (lambda () (thunk (+ n 1)))))])    ;; #2 of stream eval cons cell returns thunk w/ n + 1
    (lambda () (thunk 0))))                       ;; delayed eval of the thunk call
    

;; (9) vector-assoc - vector-flavored 'assoc'
(define (vector-assoc v vec)
  (letrec 
      ([loop (lambda(n)
             (if (= (vector-length vec) n)            ;; already processed the entire vector?
                 #f                                   ;; nothing to do
                 (let ([i (vector-ref vec n)])        ;; get ref to the ith element
                   (if (equal? (car i) v)             ;; is this ref the right element?
                       i                              ;; yes, return
                       (loop (+ n 1))))))])           ;; otherwise, keep searching
    (loop 0)))
 
;; (10) Returns a function whose behavior is the same as 'assoc', but leverages a cache of size 'n'
(define (cached-assoc xs n)
  
  ;; vector used for caching
  (define cache (make-vector n (cons #f #f)))                   
  
  ;; montonically increasing counter for tracking the next cache slot to use
  (define index 0)                                    
  
  ;; Helper function to fetch an element from the cache (if it exists)
  (define (query-cache elem)             
    (let ([result (vector-assoc elem cache)])
      (displayln result)
      (if result
          result
          #f)))
  
  ;; Helper function to update the cache (implicitly in the event of a miss)
  (define (update-cache elem)
    (vector-set! cache (modulo index n) elem)
    (set! index (add1 index))
    elem)
  
  ;; And, here's the function to return tying it all together
  (lambda (v)
    (cond [(query-cache v)]
          [(assoc v xs) => update-cache]
          [#t #f])))
    
    
    