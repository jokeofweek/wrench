#lang racket

(require racket/tcp)
(require racket/class)
(require racket/file)
(require racket/path)
(require net/uri-codec)
(require "mime.rkt")

; Hard-coded properties
(define content-root "./www")

; HTTP Request Class
(define HTTP-Request
  (class object%
    (init-field (method 'GET))
    (init-field (uri ""))
    (super-new)))

; HTTP Response Class
(define HTTP-Response
  (class object%
    (init-field (status-code 200))
    (init-field (status-message #f))
    (init-field (content-type "text/html"))
    (init-field (content ""))
    (init-field (content-callback #f))
    (super-new)))

; This function allows you to map certain URIs to others
(define (process-uri uri)
  (if (string=? uri "/")
      "/index.html"
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
    ; Attempt to get the URI
    (let ([uri (process-uri (uri-decode (read-until-whitespace in)))])
      (set-field! uri request uri))
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

; For now this uses some basic hand-coded MIME types. Look into using (read-mime-types)
;   http://docs.racket-lang.org/web-server-internal/mime-types.html
(define (update-content-type response path)
  (let ([extension (filename-extension path)])
    (when extension
      (let ([symbol-extension (string->symbol (bytes->string/utf-8 extension))])
        (when (hash-has-key? *mime-types* symbol-extension)
          (set-field! content-type response (hash-ref *mime-types* symbol-extension)))))))

; Server lambda
(define (start-server port-number)
  
  ; Request Handler
  (define (handle request)
    (let ([path (normalize-path (string-append content-root
                                               (get-field uri request)))])
      ; Make sure the file exists, or else return a 404
      (unless (file-exists? path)
        (raise
         (new HTTP-Response (status-code 404))))
      ; Build response and object and return it
      (let ((response (new HTTP-Response)))
        ; Set up content type based on path
        (update-content-type response path)
        (set-field! content
                    response 
                    (file->bytes path))
        response)))
 
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
