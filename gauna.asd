(defsystem "gauna"
  :description "Minimal and concise deep learning framework for Common Lisp"
  :author "Akira Tempaku <paku@skyizwhite.dev>"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("gauna/main")
  :in-order-to ((test-op (test-op gauna-test))))
