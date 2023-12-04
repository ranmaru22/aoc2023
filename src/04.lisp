(defpackage #:aoc2023-04
  (:nicknames #:day4)
  (:use #:cl #:alexandria))

(in-package #:aoc2023-04)

(defun make-ticket (line)
  (flet ((parse (lst) (mapcar #'parse-integer  (remove-if #'str:emptyp lst))))
    (destructuring-bind (card winning-nums ticket-nums)
        (ppcre:split "[|:]" line)
      (declare (ignorable card))
      (list
       (parse (ppcre:split "\\s+" winning-nums))
       (parse (ppcre:split "\\s+" ticket-nums))))))

(defun fold-to-score (ticket)
  (let ((winning-nums (car ticket))
        (ticket-nums  (cadr ticket))
        (score        0))
    (dolist (num ticket-nums score)
      (when (member num winning-nums) (incf score)))))

(defun count-tickets (tickets &key (total 0) count)
  (if (null tickets)
      total

      (flet ((update-count (n multi cur-count)
               (loop :for k :from 0 :below (max n (length cur-count))
                     :collect
                     (let ((cur (nth k cur-count)))
                       (cond
                         ((and cur (< k n)) (+ multi cur))
                         (cur cur)
                         ((< k n) multi))))))

        (let ((wins  (fold-to-score (car tickets)))
              (multi (1+ (or (car count) 0))))
          (count-tickets
           (cdr tickets)
           :total (+ total multi)
           :count (update-count wins multi (cdr count)))))))

(defun solve-1 ()
  (reduce
   #'+
   (mapcar
    (compose
     (lambda (score) (if (> score 0) (expt 2 (1- score)) 0))
     #'fold-to-score
     #'make-ticket)
    (uiop:read-file-lines "./input/04.txt"))))

(defun solve-2 ()
  (count-tickets (mapcar #'make-ticket (uiop:read-file-lines "./input/04.txt"))))
