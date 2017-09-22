#lang racket

(require "../common/uw-api.rkt")

(provide room-course all-courses)

;; INTEGRITY STATEMENT (modify if neccessary)
;; I received help from the following sources:
;; None. I am the sole author of this work 

;; sign this statement by removing the line below and entering your name
;; Name: Zonghan Xu
;; login ID: Z246XU

;;;;;;;;;;;;;;;;
;; INTERFACE: ;;
;;;;;;;;;;;;;;;;

;; (room-course weekday time building room) consumes a weekday, time, building
;; and room number, then it returns what class it is inside the classroom, and
;; who the instructor is.
;; room-course: Int Str Str Str -> Str
;; Requires: weekday must be one of: 1, 2, 3, 4, 5
;;           time must be in form of "xx:xx"(24 hours)
;;           must input valid building and room number(must exist)
;;           It must be a classroom: At least one class should use the room

;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION: ;;
;;;;;;;;;;;;;;;;;;;;;

;; (find-spec info-al info-type) returns the item in info-al
;; with same key with info-type
;; find-spec: AL Str -> Any

(define(find-spec info-al info-type)
  (cond
   [(empty? info-al) false]
   [(string=? info-type (first(first info-al)))
    (second(first info-al))]
   [else (find-spec (rest info-al) info-type)]))


;;MAIN PROGRAM (WRAPPER)

(define(room-course weekday time building room)
  (finalize(courses-at-that-time (all-courses building room) weekday time)))

(define(all-courses building room)
  (uw-api (string-append "/buildings/"
                         building
                         "/"
                         room
                         "/courses")))

(define(courses-on-that-day course-al weekday)
  (filter (lambda (ele) 
            (member? weekday
                     (convert-weekday 
                      (string-to-lostring 
                       (find-spec ele "weekdays")))))
          course-al))

(define (member? ele lst)
  (false?(not(member ele lst))))
                
(define(string-to-lostring string)
  (map (lambda (ele) (list->string (list ele))) (string->list string)))

(define(convert-weekday wkd)
  (cond
   [(empty? wkd) empty]
   [(string=? "M" (first wkd))
    (cons 1 (convert-weekday (rest wkd)))]
   [(and(string=? "T" (first wkd))
        (empty? (rest wkd)))
    (cons 2 (convert-weekday (rest wkd)))]
   [(and(string=? "T" (first wkd))
        (string=? "h" (second wkd)))
    (cons 4 (convert-weekday (rest(rest wkd))))]
   [(string=? "T" (first wkd))
    (cons 2 (convert-weekday (rest wkd)))]
   [(string=? "W" (first wkd))
    (cons 3 (convert-weekday (rest wkd)))]
   [(string=? "F" (first wkd))
    (cons 5 (convert-weekday (rest wkd)))]
   [else (convert-weekday (rest wkd))]))

(define(time-to-num time)
  (string->number(string-append(substring time 0 2)
                               (substring time 3 5))))

(define(courses-at-that-time course-al weekday time)
  (cond
   [(empty? course-al)  "There's never a class in there"]
   [else (filter (lambda (ele)
                   (and(>= (time-to-num time)
                           (time-to-num (find-spec ele "start_time")))
                       (<= (time-to-num time)
                           (time-to-num (find-spec ele "end_time")))))
                 (courses-on-that-day course-al weekday))]))

(define(finalize course-in-lst)
  (cond
   [(empty? course-in-lst) "There's no a class in there"]
   [else (string-append(find-spec (first course-in-lst) "subject")
                       (find-spec (first course-in-lst) "catalog_number")
                       " "
                       (find-spec (first course-in-lst) "title")
                       " "
                       "("
                       (find-spec (first course-in-lst) "section")
                       ") "
                       "Instructor: "
                       (give-inst 
                        (find-spec (first course-in-lst) "instructors")))]))

(define(give-inst inst)
  (cond
   [(empty? inst) "Staff"]
   [else (first inst)]))
  

   
