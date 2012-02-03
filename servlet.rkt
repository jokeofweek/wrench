#lang racket

(require "http.rkt")

(provide Servlet)

; Hard-coded properties
(define content-root "./www/")

; A basic servlet reads the URI and fetches the file.
(define Servlet
  (class object%
    (define/public (handle request)
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
    (super-new)))
      