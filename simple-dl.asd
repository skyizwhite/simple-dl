(defsystem "simple-dl"
  :description "Simple deep learning framework based on cl-simple-matrix"
  :author "Akira Tempaku <paku@skyizwhite.dev>"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("simple-dl/pkg"))

(register-system-packages "simple-matrix" '(:cl-simple-matrix))
