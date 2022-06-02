
;;
;; Chicken Scheme interface to the POSIX Message Queue API.
;;
;; Copyright 2021 Ivan Raikov.
;;
;; Based in part on code from the mq command line tool to use Posix Message Queues.
;; https://github.com/rotty/mq
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above
;;  copyright notice, this list of conditions and the following
;;  disclaimer in the documentation and/or other materials provided
;;  with the distribution.
;; 
;;  - Neither name of the copyright holders nor the names of its
;;  contributors may be used to endorse or promote products derived
;;  from this software without specific prior written permission.
;; 
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;  POSSIBILITY OF SUCH DAMAGE.


(module posix-mq

 (posix-mq? mq-create mq-unlink mq-info mq-send mq-recv)

 (import scheme (chicken base) (chicken foreign) (chicken condition) (chicken blob)
         (only (chicken string) ->string)
         (only srfi-1 filter) srfi-4
         (only (chicken file posix) perm/irwxu perm/irgrp perm/iroth
               open/rdonly open/rdwr open/creat open/excl open/trunc open/nonblock)
	  )

; Include into generated code, but don't parse:
#>

/* Adapted from the Ocaml function convert_flag_list: */

int convert_flag_list (C_word list)
{
   C_word l; int res;

  res = 0;
  for (l = list; !(C_truep(C_i_null_list_p(l))); l = C_u_i_cdr(l)) 
  {
    res |= C_unfix(C_u_i_car(l));
  }

  return res;
}


#ifdef HAVE_POSIX_MQ
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */
#include <mqueue.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>
#include <poll.h>
#endif


/**********************************************************************/
/* POSIX message queue                                                */
/**********************************************************************/

/* This is from the POSIX realtime extensions. Not every POSIX-type OS
 * supports it.
 */

static C_word have_posix_mq =
#ifdef HAVE_POSIX_MQ
    C_SCHEME_TRUE;
#else
    C_SCHEME_FALSE;
#endif



C_word chicken_mq_open(char *name, C_word oflags, int perm, int maxmsg, int msgsize, int *rc)
{
    C_word result;
#ifdef HAVE_POSIX_MQ
    int ret;

    int flags = 0;
    flags = convert_flag_list(oflags);

    struct mq_attr attr;
    attr.mq_flags = 0;
    attr.mq_maxmsg = maxmsg;
    attr.mq_msgsize = msgsize;
    attr.mq_curmsgs = 0;

    mqd_t queue = mq_open(name, flags, perm, &attr);

    if (queue == -1) 
    {
      *rc = -1;
      result = C_SCHEME_FALSE;
    } else
      {
         *rc = 0;
         result = C_fix(queue);
      }
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);
}


C_word chicken_mq_unlink(char *name, int *rc)
{
    C_word result;
#ifdef HAVE_POSIX_MQ
    int ret;
    *rc = 0;

    ret = mq_unlink(name);

    if (ret == -1) 
    {
      *rc = -2;
      result = C_SCHEME_FALSE;
    } else
      {
         result = C_fix(ret);
      }
    result = C_fix(ret);
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);

}


C_word chicken_mq_send(char *name, C_word oflags, C_word message, int priority, int *rc)
{
    C_word result;
#ifdef HAVE_POSIX_MQ
    int ret;
    *rc = 0;

    int flags = 0;
    flags = convert_flag_list(oflags);

    struct mq_attr attr;

    mqd_t queue = mq_open(name, O_WRONLY | flags);

    if (queue == -1) 
    {
      *rc = -1;
    } else
      {
          char * msgbuf; int msglen = 0;

          C_i_check_bytevector (message);
          msglen = C_header_size (message);
          msgbuf = C_c_bytevector (message);
          int ret = mq_send(queue, msgbuf, msglen, priority);
          if (ret != 0) 
          {
              mq_close(queue);
              *rc = -3;
              result = C_SCHEME_FALSE;
                       
          }
          else
          {
              result = C_SCHEME_TRUE;
                     mq_close(queue);
          }
       }
        
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);
}


C_word chicken_mq_recv(char *name, C_word oflags, int *rc)
{
    C_word result;
#ifdef HAVE_POSIX_MQ
    int ret;
    *rc = 0;

    int flags = 0;
    flags = convert_flag_list(oflags);

    struct mq_attr attr;

    mqd_t queue = mq_open(name, O_RDONLY | flags);

    if (queue == -1) 
    {
      *rc = -1;
      result = C_SCHEME_FALSE;
      
    } else
      {
          char * msgbuf; int msglen = 0;

  	  ret = mq_getattr(queue, &attr);
	  if (ret != 0) {
              mq_close(queue);
              *rc = ret;
              C_return(C_SCHEME_FALSE);
  	  }
         
          msglen = attr.mq_msgsize;
          msgbuf = malloc(msglen);
          if (msgbuf == NULL)
          {
              mq_close(queue);
              *rc = -1;
              C_return(C_SCHEME_FALSE);
          }

          int ret = mq_receive(queue, (void *)msgbuf, msglen, NULL);
          if (ret < 0) 
          {
              mq_close(queue);
              free(msgbuf);
              *rc = -4;
              C_return(C_SCHEME_FALSE);
          }
          C_word *a;
          a = C_alloc (C_SIZEOF_VECTOR(ret));
          result = C_bytevector(&a, ret, msgbuf);
          mq_close(queue);
          free(msgbuf);
       }
        
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);
}



<#

(define chicken_mq_info (foreign-primitive scheme-object ((nonnull-c-string name) (s32vector rc))
#<<EOF
    C_word result;
#ifdef HAVE_POSIX_MQ
    int ret;
    *rc = 0;

    struct mq_attr attr;

    mqd_t queue = mq_open(name, O_RDONLY);

    if (queue == -1) 
    {
      *rc = -1;
      result = C_SCHEME_FALSE;
    } else
      {
          int ret = mq_getattr(queue, &attr);
          if (ret != 0) 
          {
              mq_close(queue);
              *rc = ret;
              result = C_SCHEME_FALSE;
          }
          else
          {
            C_word *a;
            a = C_alloc (C_SIZEOF_LIST(3));
            result = C_list(&a, 3, C_fix(attr.mq_maxmsg), C_fix(attr.mq_msgsize), C_fix(attr.mq_curmsgs));

            mq_close(queue);
          }
       }
        
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);
EOF
))

(define (error-code-string code)
   (case code
      ((-1) "mq open error")
      ((-2) "mq unlink error")
      ((-3) "mq send error")
      ((-4) "mq receive error")
      (else "unknown mq error")))


(define (mq-check-error loc rc)
  (if (not (= rc 0))
      (abort
       (make-composite-condition
        (make-property-condition 'exn 'message (error-code-string rc))
        (make-property-condition 'posix-mq)))
      rc))

(define posix-mq? (foreign-value  "have_posix_mq"  bool))

(define chicken_mq_open (foreign-safe-lambda scheme-object "chicken_mq_open" nonnull-c-string scheme-object int int int s32vector))
(define chicken_mq_send (foreign-safe-lambda scheme-object "chicken_mq_send" nonnull-c-string scheme-object scheme-object int s32vector))
(define chicken_mq_recv (foreign-safe-lambda scheme-object "chicken_mq_recv" nonnull-c-string scheme-object s32vector))
(define chicken_mq_unlink (foreign-safe-lambda scheme-object "chicken_mq_unlink" nonnull-c-string s32vector))

(define valid-open-flags (list open/rdonly open/rdwr open/creat open/excl open/nonblock))

(define valid-send-flags (list open/nonblock))
(define valid-recv-flags (list open/nonblock))

(define (mq-create path maxmsg msgsize #!key (oflags (list)) (mode (+ perm/irwxu perm/irgrp perm/iroth)))
  (let ((rc (make-s32vector 1))
        (oflags1 (filter (lambda (x) (member x valid-open-flags)) (cons open/creat oflags))))
     (let ((res (chicken_mq_open path oflags1 mode maxmsg msgsize rc)))
        (mq-check-error 'mq-create (s32vector-ref rc 0))
        res)))

(define (mq-unlink path)
  (let ((rc (make-s32vector 1)))
     (let ((res (chicken_mq_unlink path rc)))
        (mq-check-error 'mq-unlink (s32vector-ref rc 0))
        res)))


(define (mq-info path)
  (let* ((rc (make-s32vector 1))
         (info (chicken_mq_info path rc)))
   (mq-check-error 'mq-info (s32vector-ref rc 0))
     `((maxmsg . ,(car info))
       (msgsize . ,(cadr info))
       (curmsgs . ,(caddr info)))))

(define (mq-send path message #!key (oflags (list 0)) (priority 0))
  (let ((rc (make-s32vector 1))
        (oflags1 (filter (lambda (x) (member x valid-send-flags)) oflags)))
    (let ((res (chicken_mq_send path oflags1 message priority rc)))
        (mq-check-error 'mq-send (s32vector-ref rc 0))
        res)))
 

(define (mq-recv path #!key (oflags (list 0)))
  (let ((rc (make-s32vector 1))
        (oflags1 (filter (lambda (x) (member x valid-recv-flags)) oflags)))
    (let ((res (chicken_mq_recv path oflags1 rc)))
        (mq-check-error 'mq-recv (s32vector-ref rc 0))
        res)))



)
