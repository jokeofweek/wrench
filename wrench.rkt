#lang racket

(require racket/tcp)
(require racket/class)
(require racket/file)
(require racket/path)
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
    (display "Server starting...")
    (thread loop))
  (lambda ()
    (custodian-shutdown-all server-cust)
    (display "Server shutting down...")))

; Start the server with a command loop.
(define s (start-server 80))
(let loop ([val (read-line)])
  (if (string=? val "stop")
      (begin
        (s))
      (loop (read-line))))