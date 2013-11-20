;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to (could have been WAY better doc'd!!)
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        ;; CHANGE add more cases here
        
        [(int? e) e]
        
        [(fun? e) (closure env e)]
        
        [(closure? e) e]
        
        [(aunit? e) e]
        
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (not (apair? v))
               (error "MUPL fst applied to non-apair")
               (apair-e1 v)))]
        
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (not (apair? v))
               (error "MUPL fst applied to non-apair")
               (apair-e2 v)))]
        
        [(ifgreater? e)
         (let 
             ([v1 (eval-under-env (ifgreater-e1 e) env)]
              [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (not (and (int? v1) (int? v2)))                      ;; if both not ints
               (error "MUPL ifgreater applied to non-numbers")      ;; error          
               (if (> (int-num v1) (int-num v2))                    ;; if e1 GT e2  
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))))]
        
        [(call? e)
         (let ([klosure (eval-under-env (call-funexp e) env)])
           (if (not (closure? klosure))
               (error "MUPL call applied to non-closure")
               (let* ([func (closure-fun klosure)]                              
                      [name (fun-nameopt func)]
                      [formalparams (fun-formal func)]
                      [actualparams (eval-under-env (call-actual e) env)]
                      [body (fun-body func)]
                      [environment ;; (1) extract env from closure
                       (closure-env klosure)]
                      [environment ;; (2) if anon, extend env to bind f to closure                                              
                       (if (not name)
                           environment
                           (append environment (list (cons name klosure))))]
                      [environment ;; (3) extend env with formal params                                             
                       (append (list (cons formalparams actualparams)) environment)])
                 (eval-under-env body environment))))]
        
        [(mlet? e)
         (letrec ([name (mlet-var e)]
                  [expression (eval-under-env (mlet-e e) env)]
                  [environment (append env (list (cons name expression)))]
                  [body (mlet-body e)])
           (eval-under-env body environment))]
          
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) 
                 e2 
                 e3))

(define (mlet* lstlst e2) ; lstlst is such an intuitive name! /sarcasm
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list 
          (cons "_x" e1) 
          (cons "_y" e2))
         (ifgreater (add (ifgreater (var "_x") (var "_y") (int 1) (int 0))
                         (ifgreater (var "_y") (var "_x") (int 1) (int 0))) 
                    (int 0)
                    e4
                    e3)))

;; Problem 4

(define mupl-map 
  (fun "functionname" "formalparams" 
       (fun "map" "list" 
            (ifaunit (var "list")                                            ;; if list is empty
                     (aunit)                                                 ;; return empty list
                     (apair (call (var "formalparams") (fst (var "list")))   ;; call on first element
                            (call (var "map") (snd (var "list"))))))))       ;; recurst on the remainder of the list

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "element" (call (var "map") 
                          (fun #f "y" (add (var "element") (var "y")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
