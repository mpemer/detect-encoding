(defpackage :detect-encoding/test/main
  (:use :common-lisp :detect-encoding/core/main)
  (:import-from :util/core/main :create-mock-file
                 :import-original-symbols)
  (:import-from :parachute :define-test :is :fail))

;; (defpackage :detect-encoding/test/main
;;   (:use :common-lisp :detect-encoding/core/main)
;;   (:import-from :util/core/main :create-mock-file
;;                 :import-original-symbols)
;;   (:import-from :parachute :define-test :is :fail))


(in-package #:detect-encoding/core/main)

 ;; (let ((symbols-to-import '(define-test is fail)))
 ;;   (dolist (sym symbols-to-import)
 ;;     (let ((external-symbol (find-symbol (symbol-name sym) #:parachute)))
 ;;       (when external-symbol
 ;;         (import external-symbol *package*)))))


;;(import '(define-test is fail) 'parachute)

(use-package :detect-encoding/test/main)

(define-test is-ascii-test
  (parachute:true (is-ascii #(65 66 67)))
  (parachute:false (is-ascii #(195 168)))
  (parachute:true (is-ascii #())))



(define-test has-utf8-bom-test
  ;; Test case where the bytes start with UTF-8 BOM
  (parachute:true (has-utf8-bom '(#xEF #xBB #xBF #x41 #x42 #x43))
                  "Bytes with UTF-8 BOM should return true.")

  ;; Test case where the bytes do not start with UTF-8 BOM
  (parachute:false (has-utf8-bom '(#x41 #x42 #x43 #xEF #xBB #xBF))
                   "Bytes without UTF-8 BOM at the start should return false.")

  ;; Test case with empty list
  (parachute:false (has-utf8-bom '())
                   "Empty byte list should return false.")

  ;; Test case with less than 3 bytes
  (parachute:false (has-utf8-bom '(#xEF #xBB))
                   "Byte list with less than 3 bytes should return false."))



(define-test contains-iso-8859-2-specific-characters-p-test
  ;; Test case where the bytes contain ISO-8859-2 specific characters
  (parachute:true (contains-iso-8859-2-specific-characters-p #(#xa5 #xa3 #xb1))
                  "Bytes with ISO-8859-2 specific characters should return true.")

  ;; Test case where the bytes do not contain ISO-8859-2 specific characters
  (parachute:false (contains-iso-8859-2-specific-characters-p #(#x41 #x42 #x43))
                   "Bytes without ISO-8859-2 specific characters should return false.")

  ;; Test case with empty list
  (parachute:false (contains-iso-8859-2-specific-characters-p #())
                   "Empty byte list should return false."))




(define-test detect-encoding-test
  (let ((filename (create-mock-file #(0xEF 0xBB 0xBF 0x41 0x42 0x43)))) ; UTF-8 BOM
    (parachute:is = :UTF-8 (detect-encoding filename))
    (delete-file filename)))



