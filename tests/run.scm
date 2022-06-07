(import scheme (chicken format) (chicken random) (chicken file posix) (chicken string) (chicken blob) test posix-mq)

; 
; (print posix-mq?)
; (let* ((str "Hello, world!")
      ;  (path (sprintf "/mqtest~A" (pseudo-random-integer 100)))
      ;  (fd (mq-create path  10 1000 oflags: (list open/rdwr)))
      ;  (msg (string->blob str)))
    ; (print (mq-info path)) ;; -> this makes test fail
    ; (print "sending " msg)
    ; (mq-send path msg)
    ; (print "received " (mq-recv path))
    ; (mq-unlink path)
  ; )
; 

(define msg "Hello, world!")
(define encoded-msg (string->blob msg))
(define *default-path* (sprintf "/mqtest~A" (pseudo-random-integer 100000000)))

(define create-path
  (lambda (path) (mq-create path  10 1000 oflags: (list open/rdwr))))
(define info-path
  (lambda (path) (mq-info path)))
(define send-path
  (lambda (path) (mq-send path encoded-msg)))
(define recv-path
  (lambda (path) (blob->string(mq-recv path encoded-msg))))
(define delete-path
  (lambda (path) (mq-unlink path)))

(define test-mq-recv?
  (lambda (path) (
	      (let (
		    (fd (create-path path))
		    (sd (send-path path))
		    (id (info-path path))
		    (decoded-msg (recv-path path))
		    (dd (delete-path path))
		    )
		decoded-msg
		))))


(define test-mq-info?
  (lambda (path) (
	      (let* (
		    (fd [create-path path] )
		    (sd [send-path path])
        (id [info-path path]) 
        )
        (length id))
		    )
      id
		))


(test-group "receive"
  (test-assert "received message should be equal to sent msg"
	(test-mq-recv? *default-path*) )
  )

(test-group "info"
  (test-assert "holds three pairs"
	 (test-mq-info? *default-path*)  ) )


(test-exit)
