(defpackage #:aoc2023-02
  (:nicknames #:day2)
  (:use #:cl))

(in-package #:aoc2023-02)

(defun parse-game (game)
  (let ((id     (parse-integer (ppcre:scan-to-strings "^(\\d+)(?=:)" game)))
        (reds   (mapcar #'parse-integer (ppcre:all-matches-as-strings "(\\d+)(?= red)" game)))
        (greens (mapcar #'parse-integer (ppcre:all-matches-as-strings "(\\d+)(?= green)" game)))
        (blues  (mapcar #'parse-integer (ppcre:all-matches-as-strings "(\\d+)(?= blue)" game))))
    (list id reds greens blues)))

(defun game-possible-p (game max-red max-green max-blue)
  (and (every (lambda (x) (<= x max-red))   (cadr game))
       (every (lambda (x) (<= x max-green)) (caddr game))
       (every (lambda (x) (<= x max-blue))  (cadddr game))
       (car game)))

(defun smallest-possible-cubes (game)
  (mapcar (lambda (x) (apply #'max x)) (cdr game)))

(defun sum-up-with (file fn)
  (loop :for line := (read-line file nil nil)
        :while line
        :sum (funcall fn line)))

(defun solve-1 ()
  (with-open-file (in "./input/02.txt")
    (sum-up-with in (lambda (line) (or (game-possible-p (parse-game line) 12 13 14) 0)))))

(defun solve-2 ()
  (with-open-file (in "./input/02.txt")
    (sum-up-with in (lambda (line) (reduce #'* (smallest-possible-cubes (parse-game line)))))))
