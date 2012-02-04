#lang racket

(require racket/file)

(provide
 *config*
 load-config
 get-config-value)

; Load the configuration file if it exists
(define *config* '())

; Set up the default configuration
(define *default-config*
  '((server-port . 80)
    (content-path . "www/")))

; Simpler syntax for buildin
(define (get-config-value value)
  (if (assoc value *config*)
      (cdr (assoc value *config*))
      (if (assoc value *default-config*)
          (cdr (assoc value *default-config*))
          #f)))

; Load the configuration
(define (load-config)
  (set! *config*
        (if (file-exists? "wrench.conf")
            (let ([pattern  #rx"(.*) = (.*)"])
              (let loop ([lines (file->lines "wrench.conf")]
                         [config '()])
                (if (null? lines)
                    config
                    (let ([values (regexp-match pattern (car lines))])
                      (let ([in (open-input-string (caddr values))])
                        (let ([value (read in)])
                          (close-input-port in)
                          (loop (cdr lines)
                                (append 
                                 config
                                 (list (cons (string->symbol (cadr values) )
                                             value))))))))))
            '()))
  (display "Configuration loaded...\r\n"))

(load-config)