#lang racket

(require "servlet.rkt")

(provide get-mapped-uri
         get-mapped-servlet)

; General file servlet
(define *file-servlet* (new Servlet))

; This method allows you to map certain URLs to other URLs
(define (get-mapped-uri uri)
  (if (string=? uri "")
      "index.html"
      uri))

; This method allows you to map certian URLs to servlets
(define (get-mapped-servlet uri)
  *file-servlet*)