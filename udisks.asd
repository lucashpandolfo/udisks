;;;; udisks.asd

(asdf:defsystem #:udisks
  :serial t
  :description "Describe udisks here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:dbus)
  :components ((:file "package")
               (:file "conversions")
               (:file "udisks")
               (:file "block")))

