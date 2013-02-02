(in-package #:udisks)

(defun string->keyword (string)
  (if (> (length string) 0)
      (intern (string-upcase string) :keyword)
      nil))

(defun bytes->string (seq)
  (flexi-streams:octets-to-string (butlast seq) :external-format :utf-8))
