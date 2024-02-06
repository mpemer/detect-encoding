;;;; detect-encoding.asd

(defsystem "detect-encoding"
  :class :package-inferred-system
  :version (:read-file-line "version.txt" :at 0)
  :author "Marcus Pemer"
  :license "GPL-3.0"
  :depends-on ("parachute" "util" "detect-encoding/core/main")
  :in-order-to ((test-op (load-op "detect-encoding/test/main")))
  :perform (test-op (o c) (symbol-call :parachute 'test 'detect-encoding/core/main))
  :description "Description of your TDA system."
  :long-description "Longer description of your TDA system."
  :maintainer "marcus@pemer.com"
  :homepage "https://github.com/mpemer/detect-encoding")

;;(register-system-packages :util/core/main '(:util))
;;(register-system-packages :detect-encoding/core/main '(:detect-encoding))
