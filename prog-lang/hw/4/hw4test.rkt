#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")
(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence tests
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "sample 1")
   (check-equal? (sequence 3 8 3) (list 3 6) "sample 2")
   (check-equal? (sequence 3 2 1) null "sample 3")
   (check-equal? (sequence 1 2 4) (list 1) "stride exceeds range")

   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   (check-equal? (string-append-map
                  (list "foo" "bar" "baz")
                  "") '("foo" "bar" "baz"))
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 0) 0 "beginning edge case")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 4) 4 "end edge case")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 5) 0 "overlap")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 7) 2 "overlap #2")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 1) (list 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 3) (list 1 1 1) "greater than 1")
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 0) null "nil case")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg") "dan-then-dog test #2")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 0) null 
                 "empty case test")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   (check-equal? (vector-assoc 9 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "DNE") 
   (check-equal? (vector-assoc 4 (make-vector 0 #f)) #f "empty vector case")
   
   ; cached-assoc `tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 9) #f "negative case")
   (check-equal? ((cached-assoc null 3) 42) #f "negative case on an empty list")
   
   ; while-less test
   ;;(check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
