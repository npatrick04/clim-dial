;;;; dial.asd

(asdf:defsystem #:dial
  :description "Describe dial here"
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:mcclim)
  :components ((:file "package")
               (:file "dial")))

