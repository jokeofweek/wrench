#lang racket

(require "http.rkt")

(provide Servlet
         Reflector-Servlet)

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

; A basic reflector servlet which prints the Racket serialized version of the request and response.
; This also shows an example of using content-callback.
(define Reflector-Servlet
  (class Servlet
    (define/override (handle request)
      (new HTTP-Response
           (content-callback
            (lambda (response)
              (define (reflect object)
                (for-each
                 (lambda (sym)
                   (let ([val (eval `(get-field ,sym ,object))])
                     (write (cons sym 
                                (if (null? val)
                                    '(())
                                    val))))
                   (display "<br/>"))
                 (field-names object)))
              (display "<h1>Reflected data:</h1><br/><hr/>")
              (display "<h2>Request:</h2><br/>")
              (reflect request)
              (display "<hr/><h2>Response:</h2><br/>")
              (reflect response)))))
    (super-new)))
           