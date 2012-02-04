#lang racket

(require racket/tcp)
(require racket/class)
(require racket/file)
(require racket/path)
(require "config.rkt")
(require "http.rkt")
(require "mappings.rkt")

; Server lambda
(define (start-server port-number)
  ; Request Handler
  (define (handle request)
    (send (get-mapped-servlet (get-field uri request)) 
          handle 
          request))
  ; Connection Request
  (define (accept-connection listener)
    (define cust (make-custodian))
    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))
      ; Set up handling thread
      (define handle-thread 
        (thread
         (lambda ()
           (define response #f)
           (with-handlers
               ([(lambda (v)
                   (is-a? v HTTP-Response))
                 (lambda (v)
                   (set! response v))])
             (let ([request (read-request-object get-mapped-uri in)])
               (set! response (handle request))))
           (display-response response out)
           (close-input-port in)
           (close-output-port out))))
      ; Timeout after 30 seconds
      (thread 
       (lambda ()
         (sleep 30)
         (custodian-shutdown-all cust)
         (kill-thread handle-thread)))))
  
  ; Start the server loop and return the stop server callback
  (define server-cust (make-custodian))
  (parameterize ([current-custodian server-cust])
    (define listener (tcp-listen port-number 5 #t))
    (define (loop)
      (accept-connection listener)
      (loop))
    (display "Server starting...\r\n")
    (thread loop))
  (lambda ()
    (custodian-shutdown-all server-cust)
    (display "Server shutting down...\r\n")))

(define s (start-server (get-config-value 'server-port)))

(let loop ([val (read-line)])
  (cond 
    ([or (string=? val "help") (string=? val "help\r")]
     (display "Wrench Commands:\r\n")
     (display "\tmemory - displays memory usage.\r\n")
     (display "\tstop - stops the server and exists the applicaton.\r\n")
     (newline))
    ([or (string=? val "stop") (string=? val "stop\r")]
     (s)
     (exit))
    ([or (string=? val "memory") (string=? val "memory")]
     (display "Currently using ~")
     (display (current-memory-use))
     (display " bytes. \r\n")))
  (loop (read-line)))