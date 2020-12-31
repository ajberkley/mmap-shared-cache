(defpackage :mmap-shared-cache
  (:documentation
   "Provides low level access to a shared mmap file.  Not intended for general purpose use as it's
    almost never a great idea.  There is a minor sbcl specific function here for convenience.")
  (:use :common-lisp)
  (:export))

(in-package :mmap-shared-cache)

(defun delete-cache-file (filename)
  (delete-file filename))

(defun mmap-file (path &key (input t) (output nil) (exclusive nil) (create nil) (size nil))
  "If you do create you need input t also..."
  (let ((fd (osicat-posix:open path (logior (if exclusive osicat-posix:o-excl 0)
                                            (if create osicat-posix:o-creat 0)
                                            (if (and input (not output))
                                                osicat-posix:o-rdonly
                                                osicat-posix:o-rdwr)))))
    (unwind-protect
         (let* ((size (if (and size create)
                          (progn (osicat-posix:ftruncate fd size) size)
                          (let ((file-size (osicat-posix:stat-size (osicat-posix:fstat fd))))
                            (when size (assert (>= file-size size)))
                            file-size)))
                (addr (osicat-posix:mmap (cffi:null-pointer) size
                                         (logior (if input osicat-posix:prot-read 0)
                                                 (if output osicat-posix:prot-write 0))
                                         (logior osicat-posix:map-shared)
                                         fd 0)))
           (values addr size))
      (osicat-posix:close fd))))

(defmacro with-mmapped-file ((file addr size &optional (input t) (output nil) (exclusive nil) (create nil)) &body body)
  (let ((original-addr (gensym "ADDR-"))
        (original-size (gensym "SIZE-")))
    `(let* ((,addr (mmap-file ,file :input ,input :output ,output :exclusive ,exclusive :size ,size :create ,create))
            (,original-addr ,addr)
            (,original-size ,size))
       (unwind-protect
            (progn ,@body)
         (osicat-posix:munmap ,original-addr ,original-size)))))

(defmacro with-cache-file-for-writing ((path addr size) &body body)
  "futex is 32 bits on all platforms for sbcl, need to compare and swap"
  `(with-mmapped-file (,path ,addr ,size t t t t)
     ,@body))

(defmacro with-cache-creation-func ((try-creation-function filename size &key (must-create-error 'osicat-posix:enoent) (sleep-backoff 0.1)) &body body)
  (let ((success (gensym "SUCCESS-"))
        (addr (gensym "ADDR-")))
    `(let ((,success nil))
       (loop until ,success
             repeat 100
             do
                (handler-case
                    (multiple-value-prog1
                        (progn ,@body)
                      (setf ,success t))
                  (,must-create-error ()
                    ;; In this case, we have to attempt to create the file
                    (with-cache-file-for-writing (,filename ,addr ,size)
                      (funcall ,try-creation-function ,addr ,size))))
             do (when (not ,success) (sleep ,sleep-backoff)))
       (unless ,success (error "Could not create cache file")))))

(defmacro with-mmap-shared-cache ((cache-creator cache-filename addr size) &body body)
  (alexandria:once-only (cache-filename)
    `(with-cache-creation-func (,cache-creator ,cache-filename ,size)
       (with-mmapped-file (,cache-filename ,addr ,size)
         ,@body))))

(defun write-vector-to-sap (addr vector type)
  (let ((bit-blitter (ecase (cffi:foreign-type-size type)
                       (1 #'sb-kernel::system-area-ub8-copy)
                       (2 #'sb-kernel::system-area-ub16-copy)
                       (4 #'sb-kernel::system-area-ub32-copy)
                       (8 #'sb-kernel::system-area-ub64-copy))))
    (sb-sys:with-pinned-objects (vector)
      (funcall bit-blitter (sb-sys::vector-sap vector) 0 addr 0 (length vector)))))

(declaim (inline vector-ref-sap))
(defun vector-ref-sap (addr type index)
  (cffi:mem-aref addr type index))

(defun ensure-cache-file (filename cache-writer size)
  (with-mmap-shared-cache (cache-writer filename addr size)))

(defun test-cached (filename)
  (labels ((store-cached-data (addr size)
             (format t "Writing cached data... addr ~A size ~A!~%" addr size)
             (let ((a (make-array 65536 :element-type '(unsigned-byte 16) :initial-element 1234)))
               (write-vector-to-sap addr a :uint16))))
    (with-mmap-shared-cache (#'store-cached-data filename addr (* 65536 2))
      (format t "Opened cache at ~A~%" addr)
      (format t "data: ~A...~%" (loop for offset below 8 collect (cffi:mem-aref addr :uint16 offset))))))
