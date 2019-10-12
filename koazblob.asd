(defsystem "koazblob"
  :version "0.1.0"
  :author "Koga Kazuo"
  :license "OpenBSD's BSD license"
  :depends-on (#:alexandria
               #:cl-base64
               #:cxml
               #:dexador
               #:ironclad
               #:split-sequence
               #:trivial-rfc-1123
               #:trivial-utf-8)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Microsoft Azure Blob Storage API"
  :in-order-to ((test-op (test-op "koazblob/tests"))))

(defsystem "koazblob/tests"
  :author ""
  :license ""
  :depends-on ("koazblob"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for koazblob"
  :perform (test-op (op c) (symbol-call :rove :run c)))
