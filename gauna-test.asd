(defsystem "gauna-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "gauna-test/pkg")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
