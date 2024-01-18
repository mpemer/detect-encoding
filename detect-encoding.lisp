;;;; -----------------------------------------------------------------------
;;;; Filename: detect-encoding.lisp
;;;; Author: Marcus Pemer
;;;; Email: marcus@pemer.com
;;;;
;;;; Description:
;;;; This file contains a collection of utility functions for use in Lisp
;;;; applications. It includes functions for parsing numbers, manipulating
;;;; hash tables, rounding numbers, processing lists with function chains
;;;; and monadic transformations, and other general utility functions.
;;;;
;;;; Copyright (C) 2023 Marcus Pemer
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;; -----------------------------------------------------------------------
(in-package #:detect-encoding)

;;;; -----------------------------------------------------------------------
(defun is-ascii (bytes)
  "Checks if all bytes are in the ASCII range."
  (loop for byte across bytes always (<= byte 127)))

;;;; -----------------------------------------------------------------------
(define-test is-ascii-test
  (parachute:true (is-ascii #(65 66 67)))
  (parachute:false (is-ascii #(195 168)))
  (parachute:true (is-ascii #())))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun has-utf8-bom (bytes)
  "Checks if the beginning of a byte list is the UTF-8 BOM."
  (when (>= (length bytes) 3)
    (destructuring-bind (b0 b1 b2 . _) bytes
      (and (eql b0 #xEF) (eql b1 #xBB) (eql b2 #xBF)))))

;;;; -----------------------------------------------------------------------
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
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
;; Make a lookup table
(defvar *iso-8859-2-specific-bytes*
  (let ((hash-table (make-hash-table)))
    (dolist (byte '(#xa5 #xa3 #xb1 #xb3 #xbc #xbe
                    #xd1 #xd3 #xdd #xe1 #xe3 #xe6
                    #xea #xf1 #xf3 #xf5 #xf8 #xfa)
                  hash-table)
      (setf (gethash byte hash-table) t))))
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun contains-iso-8859-2-specific-characters-p (bytes)
  "Checks if byte list contains characters specific to ISO-8859-2."
  (some (lambda (byte) (gethash byte *iso-8859-2-specific-bytes*)) bytes))

;;;; -----------------------------------------------------------------------
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
;;;; -----------------------------------------------------------------------



;;;; -----------------------------------------------------------------------
(defun detect-encoding (filename)
  "Detects encoding of a file, guessing between ASCII, UTF-8, ISO-8859-1, and ISO-8859-2."
  (handler-case
      (let ((bytes (read-file-as-bytes filename)))
        (cond ((has-utf8-bom bytes) :UTF-8)
              ((contains-iso-8859-2-specific-characters-p bytes) :ISO-8859-2)
              ((is-ascii bytes) :ASCII)
              (t :ISO-8859-1)))
    ;; Handling file-related errors
    (error (e)
      (format nil "Error reading file '~A': ~A" filename e)
      :error)))

;;;; -----------------------------------------------------------------------
(define-test detect-encoding-test
  (let ((filename (create-mock-file #(0xEF 0xBB 0xBF 0x41 0x42 0x43)))) ; UTF-8 BOM
    (parachute:is = (detect-encoding filename) :UTF-8)
    (delete-file filename)))
;;;; -----------------------------------------------------------------------
