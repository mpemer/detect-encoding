(in-package :cl-user)

(defpackage :detect-encoding
  (:use :common-lisp)
  (:import-from :util :read-file-as-bytes :create-mock-file)
  (:import-from :parachute :define-test)
  (:export :detect-encoding))
