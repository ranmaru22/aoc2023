(defpackage #:aoc2023-01
  (:nicknames #:day1)
  (:use #:cl))

(in-package #:aoc2023-01)

(defun extract-nums (str)
  (remove-if #'null
   (loop :for chr :across str
         :collect (digit-char-p chr))))

(defun extract-nums-and-words (str)
  (let* ((regexp "([1-9]|one|two|three|four|five|six|seven|eight|nine)")
         (nums '(("one" . 1) ("two" . 2) ("three" . 3) ("four" . 4) ("five" . 5)
                 ("six" . 6) ("seven" . 7) ("eight" . 8) ("nine" . 9)))
         (first (ppcre:scan-to-strings regexp str))
         (last (loop :for i :from (length str) :downto 0
                     :if (cl-ppcre:scan-to-strings regexp (subseq str i))
                       :return (cl-ppcre:scan-to-strings regexp (subseq str i)))))
    (mapcar (lambda (num) (or (cdr (assoc num nums :test #'string=)) (parse-integer num)))
            (list first last))))

(defun sum-up-with (file fn)
  (loop :for line := (read-line file nil nil)
        :while line
        :when line
          :sum (let ((nums (funcall fn line)))
                 (+ (* 10 (first nums)) (car (last nums))))))

(defun solve-1 ()
  (with-open-file (in "./input/01.txt")
    (sum-up-with in #'extract-nums)))

(defun solve-2 ()
  (with-open-file (in "./input/01.txt")
    (sum-up-with in #'extract-nums-and-words)))
