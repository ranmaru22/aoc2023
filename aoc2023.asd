(asdf:defsystem "aoc2023"
  :serial t
  :author "ranmaru22"
  :description "Advent of Code 2023"
  :depends-on (#:alexandria
               #:uiop
               #:cl-ppcre
               #:str)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "01"))
                ((:file "02")))))
