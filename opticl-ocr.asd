(defsystem "opticl-ocr"
  :version "0.1.0"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("opticl")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "CL versions of Unpaper algorithms"
  :in-order-to ((test-op (test-op "opticl-ocr/tests"))))

(defsystem "opticl-ocr/tests"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("opticl-ocr"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for opticl-ocr"
  :perform (test-op (op c) (symbol-call :rove :run c)))
