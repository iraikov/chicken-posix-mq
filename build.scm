
(import (chicken base) (chicken format) (chicken process) (chicken process-context) srfi-1 srfi-13 compile-file)
(define args (command-line-arguments))


(define (mq-try-compile ldflags cflags)
  (and (try-compile 
	(string-append "#include <sys/mman.h>\n"
		       "#include <sys/stat.h>\n"
		       "#include<fcntl.h>\n"
		       "#include<mqueue.h>\n"
		       "\n" 
		       "int main(int argc, char **argv) { mq_open(\"test\",0); return 0; }\n")
	ldflags: ldflags
	cflags: cflags)
       (cons ldflags cflags)))


(define-syntax mq-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (mq-try-compile flags ...)
		     (t ()    #f)))))

(define c+ld-options (or (mq-test ("-lrt" "-DHAVE_POSIX_MQ")) ""))

(define cmd (intersperse (append args (filter (lambda (x) x)
                                              (list (sprintf "-L \"~A\"" (car c+ld-options))
                                                    (and (> (string-length (cdr c+ld-options)) 0)
                                                         (sprintf "-C \"~A\"" (cdr c+ld-options))))))
                                 " "))
(print (string-concatenate cmd))
(system (string-concatenate cmd))
