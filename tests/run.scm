(import scheme (chicken format) (chicken random) (chicken file posix) (chicken string) (chicken blob) posix-mq)


(print posix-mq?)
(let* ((str "Hello, world!")
       (path (sprintf "/mqtest~A" (pseudo-random-integer 100)))
       (fd (mq-create path  10 1000 oflags: (list open/rdwr)))
       (msg (string->blob str)))
    (print (mq-info path)) ;; -> this makes test fail
    (print "sending " msg)
    (mq-send path msg)
    (print "received " (mq-recv path))
    (mq-unlink path)
  )


