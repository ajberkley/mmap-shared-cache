(defsystem :mmap-shared-cache
  :version "2020.12.26"
  :licence "BSD 3 clause"
  :description "A shared memory mapped disk cache"
  :author "Andrew Berkley <ajberkley@gmail.com>"
  :serial t
  :components
  ((:file "mmap-shared-cache"))
  :depends-on (:osicat :alexandria :cffi))
