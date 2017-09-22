#lang racket

;; DO NOT MODIFY THIS FILE

;; Warning! For new 136 students, there is some I/O here that
;; will not be covered until Section 07.

;; You don't have to understand how this module works
;; (psst! that's called *abstraction*)

;; The io-test module provides a universal driver for I/O testing in racket

(provide io-test)

;; a Command is one of:
;; * (list Any Nat (State ... -> State))
;; * (list Any -1)  [for terminating the test]
;; requires: for the function (State ... -> State)
;;           it consumes additional parameters equal to the value of the Nat

;; (io-test locmd initial-state) reads in values from input,
;;   processing the corresponding commands (from locmd) 
;;   moving from state to state (starting with initial-state)
;;   until a termination is reached
;; io-test: (listof Command) State -> Void

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION

(define (io-test locmd initial-state)
  ;; (get-next state) reads in a value from input,
  ;;   looks up that value in the list of Commands [locmd], 
  ;;   then performs the corresponding command function
  ;;   that either generates a new state and repeats get-next or terminates
  ;;   note: additional parameters may be read in for some commands
  
  (define (get-next state)
    (define sym (read))
    (define cmd (assoc sym locmd))
    (cond [(not cmd) (printf "unknown command: ~a\n" sym)]
          [(= -1 (second cmd)) (void)]
          [else (define params (build-list (second cmd)
                                           (lambda (p) (read))))
                (get-next (apply (third cmd) (cons state params)))]))
  (get-next initial-state))
