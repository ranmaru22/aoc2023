(defpackage #:aoc2023-05
  (:nicknames #:day5)
  (:use #:cl #:alexandria))

(in-package #:aoc2023-05)

(defun make-raw-map (mapstr)
  (let* ((raw-list (ppcre:split "\\n" mapstr))
         (map-name (car (ppcre:split "\\s" (car raw-list))))
         (coords   (cdr raw-list)))
    (list map-name
           (mapcar (lambda (line) (mapcar #'parse-integer  (ppcre:split "\\s" line))) coords))))

(defun make-proper-map (raw-map)
  (labels ((calculate-destination (source destination)
             (lambda (x) (+ destination (- x source))))
           (make-lookup-fn (line)
             (destructuring-bind (destination source range) line
               (list source (+ source range) (calculate-destination source destination)))))

    (let ((maplist (mapcar #'make-lookup-fn (cadr raw-map))))
      (lambda (seed)
        (let ((func (find-if (lambda (map) (<= (car map) seed (cadr map))) maplist)))
          (if func (funcall (caddr func) seed) seed))))))

(defun solve-1 ()
  (let* ((file "./input/05.txt")
         (seeds (mapcar #'parse-integer (cdr (ppcre:split "\\s" (uiop:read-file-line file)))))
         (maps
           (mapcar
            (compose #'make-proper-map #'make-raw-map)
            (reverse (cdr (ppcre:split "\\n{2}" (uiop:read-file-string file)))))))
    (apply #'min (mapcar (apply #'compose maps) seeds))))

;; Brute force solution. Should work, but my machine can't run it.
(defun solve-2 ()
  (let* ((file "./input/05.txt")
         (seed-ranges (mapcar #'parse-integer (cdr (ppcre:split "\\s" (uiop:read-file-line file)))))
         (seeds (loop :for (n k) :on seed-ranges :by #'cddr :nconc (iota k :start n)))
         (maps
           (mapcar
            (compose #'make-proper-map #'make-raw-map)
            (reverse (cdr (ppcre:split "\\n{2}" (uiop:read-file-string file)))))))
    seeds))
