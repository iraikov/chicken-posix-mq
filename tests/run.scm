(import scheme (chicken format) (chicken random) (chicken file posix) (chicken string) (chicken blob) test posix-mq)


(print posix-mq?)
(let* ((str "Hello, world!")
			 (path (sprintf "/mqtest~A" (pseudo-random-integer 100)))
			 (fd (mq-create path  10 1000 oflags: (list open/rdwr)))
			 (msg (string->blob str)))
	(print (mq-info path)) 
	(print "sending " msg)
	(mq-send path msg)
	(print "received " (mq-recv path))
	(mq-unlink path))
 

(define msg "Hello, world!")
(define encoded-msg (string->blob msg))
(define *default-path* (sprintf "/mqtest~A" (pseudo-random-integer 100)))

(define (create-queue path)
  (mq-create path  10 1000 oflags: (list open/rdwr)))
(define info-queue
  (lambda (path) (mq-info path)))
(define send-to-queue
  (lambda (path) (mq-send path encoded-msg)))
(define recv-from-queue
  (lambda (path) (blob->string(mq-recv path encoded-msg))))
(define delete-queue
  (lambda (path) (mq-unlink path)))

(define (mq-recv-aux path)
  (let ((fd (create-queue path))
				(sd (send-to-queue path))
				(id (info-queue path))
				(decoded-msg (recv-from-queue path))
				(dd (delete-queue path)))
		decoded-msg))


(define (mq-info-aux path)
 (let ((fd [create-queue path])
					 (sd [send-to-queue path])
					 (id [info-queue path]))
			 (length id)))


(test-group "receive"
  (test "received message should be equal to sent msg"
				msg
				(mq-recv-aux *default-path*)))

(test-group "info"
  (test "queue holds three pairs" 3
	 (mq-info-aux *default-path*)))


(test-exit)
