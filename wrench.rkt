#lang racket

(require racket/tcp)
(require racket/class)
(require racket/file)
(require racket/path)
(require net/uri-codec)
(require net/url)
(require "http.rkt")
(require "servlet.rkt")

; General file servlet
(define *file-servlet* (new Servlet))

; This function allows you to map certain URIs to others
(define (process-uri uri)
  (if (string=? uri "")
      "index.html"
      uri))

; Attempt to build an HTTP-Request object from a stream
(define (get-request-object in)
  (let ((request (new HTTP-Request)))
    ; Attempt to get the method
    (let ([method (string->symbol (read-until-whitespace in))])
      (case method
        ([GET POST]
         (set-field! method request method))
        (else
         (raise (new HTTP-Response (status-code 405))))))
    ; Parse the URI
    (let ([url (string->url (read-until-whitespace in))])
      (set-field! uri request (process-uri (string-join (map path/param-path (url-path url)) "/")))
      (set-field! get-parameters request (url-query url)))
    ; Make sure that the HTTP version is next, or else send bad request
    (unless (string=? (read-until-whitespace in)
                      "HTTP/1.1")
      (raise (new HTTP-Response (status-code 400))))
    
    ; Discard headers
    (regexp-match #rx"(\r\n|^)\r\n" in)
    request))

; Output an HTTP-Response object
(define (display-response response out)
  ; Get the valid message if none was specified
  (define (get-status-message status)
    (case status
      ([100] "Continue")
      ([101] "Switching Protocols")
      ([200] "OK")
      ([201] "Created")
      ([202] "Accepted")
      ([203] "Non-Authoritative Information")
      ([204] "No Content")
      ([205] "Reset Content")
      ([206] "Partial Content")
      ([300] "Multiple Choices")
      ([301] "Moved Permanently")
      ([302] "Found")
      ([303] "See Other")
      ([304] "Not Modified")
      ([305] "Use Proxy")
      ([307] "Temporary Redirect")
      ([400] "Bad Request")
      ([401] "Unauthorized")
      ([402] "Payment Required")
      ([403] "Forbidden")
      ([404] "Not Found")
      ([405] "Method Not Allowed")
      ([406] "Not Acceptable")))
  
  ; Build the status line
  (define (get-status-line status [message #f])
    (let ([message (or message 
                       (get-status-message status))])
      (string-append "HTTP/1.1 "
                     (number->string status)
                     " "
                     message
                     "\r\n")))
  
  ; Output the response
  (display (get-status-line (get-field status-code response)
                            (get-field status-message response))
           out)
  (display "Server: k\r\nContent-Type: " out)
  (display (get-field content-type response) out)
  (display "\r\n\r\n" out)
  ; If a callback was specified, call it, or else just print content
  (if (get-field content-callback response)
      ((get-field content-callback response) out)
      (display (get-field content response) out)))

; Server lambda
(define (start-server port-number)
  ; Request Handler
  (define (handle request)
    (send *file-servlet* handle request))
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
             (let ([request (get-request-object in)])
               (set! response (handle request))))
           (display-response response out)
           (close-input-port in)
           (close-output-port out))))
      ; Timeout after 30 seconds
      (thread 
       (lambda ()
         (sleep 30)
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

; I/O Helper Functions
(define (read-until-whitespace in)
    (let reader ([contents '()])
      (let ([current (read-char in)])
        (if (or (eq? current eof)
                (char-whitespace? current))
            (list->string contents)
            (reader (append contents (cons current '())))))))

; URL Helper Functions
(define (split-url-string url)
  (let loop ([remaining (string->list url)]
             [resource '()])
    (if (null? remaining)
        (values url "")
      (let ([first (car remaining)])
        (if (eq? #\? first)
            (values (list->string resource)
                    (list->string (cdr remaining)))
          (loop (cdr remaining) (append resource (cons first '()))))))))