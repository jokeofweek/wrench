#lang racket

(require "mime.rkt")

(provide HTTP-Request HTTP-Response update-content-type)

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