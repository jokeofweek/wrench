#lang racket


(require net/uri-codec)
(require net/url)
(require "mime.rkt")

(provide HTTP-Request 
         HTTP-Response
         update-content-type
         read-request-object
         display-response)

; HTTP Request Class
(define HTTP-Request
  (class object%
    (init-field (method 'GET))
    (init-field (uri ""))
    (init-field (get-parameters #f))
    (init-field (user-agent ""))
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

; Update the content type using the mime types table.
(define (update-content-type response path)
  (let ([extension (filename-extension path)])
    (when extension
      (let ([symbol-extension (string->symbol (bytes->string/utf-8 extension))])
        (when (hash-has-key? *mime-types* symbol-extension)
          (set-field! content-type response (hash-ref *mime-types* symbol-extension)))))))

; Output an HTTP-Response  object
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
  (parameterize ([current-output-port out])
    (display (get-status-line (get-field status-code response)
                              (get-field status-message response)))
    (display "Server: Wrench\r\nContent-Type: ")
    (display (get-field content-type response))
    (display "\r\n\r\n")
    ; If a callback was specified, call it, or else just print content
    (if (get-field content-callback response)
        ((get-field content-callback response))
        (display (get-field content response)))))


; Attempt to build an HTTP-Request object from a stream
(define (read-request-object uri-mapper in)
  ; Process a header pair, setting the necessary fields on a request object 
  (define (process-request-header request header)
    (when header
      (case (car header)
        ([User-Agent] (set-field! user-agent request (cdr header))))))
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
      (set-field! uri request (uri-mapper (string-join (map path/param-path (url-path url)) "/")))
      (set-field! get-parameters request (url-query url)))
    ; Make sure that the HTTP version is next, or else send bad request
    (unless (string=? (read-until-whitespace in)
                      "HTTP/1.1")
      (raise (new HTTP-Response (status-code 400))))
    
    ; Read all headers until we get an empty line
    (let reader ([line (read-line in)])
      (unless (string=? line "\r")
        (process-request-header request (build-header-pair line))
        (reader (read-line in))))
    
    request))

; I/O Helper Functions
(define (read-until-whitespace in)
    (let reader ([contents '()])
      (let ([current (read-char in)])
        (if (or (eq? current eof)
                (char-whitespace? current))
            (list->string contents)
            (reader (append contents (cons current '())))))))

; Attempts to build a header pair from a string, or else return f
;  Would return ( header . "value" )
(define (build-header-pair str)
  (if (eq? eof str)
      #f
      (let loop ([content (string->list str)]
                 [header '()])
        (if (null? content)
            #f
            (let ([first (car content)])
              (if (eq? first #\:)
                  (cons (string->symbol (list->string header))
                        (list->string (cdr content)))
                  (loop (cdr content)
                        (append header (list first)))))))))
  