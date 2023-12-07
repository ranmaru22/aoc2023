(defpackage #:aoc2023-06
  (:nicknames #:day6)
  (:use #:cl #:alexandria))

(in-package #:aoc2023-06)

(defparameter *races*
  '((40 .  233)
    (82 . 1011)
    (84 . 1110)
    (92 . 1487)))

(defparameter *race-2* '(40828492 . 233101111101487))

(defun ways-to-win (race)
  (destructuring-bind (time . dist) race
    (let* ((m  (/ (- time) 2))
           (u  (sqrt (- (expt m 2) dist)))
           (x1 (- m u))
           (x2 (+ m u)))
      (1+ (abs (- (floor (min x1 x2)) (ceiling (max x1 x2))))))))

(defun solve-1 ()
  (reduce #'* (mapcar #'ways-to-win *races*)))

(defun solve-2 ()
  (ways-to-win *race-2*))
