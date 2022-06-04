(import scheme (chicken format) (chicken random) (chicken file posix) (chicken string) (chicken blob) test posix-mq)


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


(define msg "Hello, world!")
(define encoded-msg (string->blob msg))
(define path (sprintf "/mqtest~A" (pseudo-random-integer 100000000)))

(define create-path
  (lambda () (mq-create path  10 1000 oflags: (list open/rdwr))))
(define info-path
  (lambda () (mq-info path)))
(define send-path
  (lambda () (mq-send path encoded-msg)))
(define recv-path
  (lambda () (blob->string(mq-recv path encoded-msg))))
(define delete-path
  (lambda () (mq-unlink path)))

(define test-mq-rcv
  (lambda () (
	      (let (
		    (fd (create-path))
		    (sd (send-path))
		    (id  (info-path))
		    (decoded-msg (recv-path))
		    (dd (delete-path))
		    )
		(sprintf "~A" decoded-msg)
		)
	      
	      ))

  )

(test-group "mq-recv"
  (test "received message should be equal to msg"
	msg
	(test-mq-rcv) )
  )

(test-exit)
