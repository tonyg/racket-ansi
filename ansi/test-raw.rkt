#lang racket/base

(require "main.rkt")

(require racket/set)
(require racket/match)

(define (main)
  (tty-raw!)

  (define utf-8?
    (match (current-command-line-arguments)
      [(or '#() '#("--utf-8")) #t]
      ['#("--no-utf-8") #f]
      [_ (error 'main "Usage: test-raw [ --utf-8 / --no-utf-8 ]")]))

  (plumber-add-flush! (current-plumber)
                      (lambda (handle)
                        (for-each display
                                  (list
                                   (reset-mode x11-extended-mouse-tracking-mode)
                                   (reset-mode x11-any-event-mouse-tracking-mode)
                                   (reset-mode x11-focus-event-mode)
                                   ))))

  (for-each display (list
                     (set-mode x11-focus-event-mode)
                     (set-mode x11-any-event-mouse-tracking-mode)
                     (set-mode x11-extended-mouse-tracking-mode)
                     ))

  (display "Type keys. Press control-D to exit.\r\n")
  (let loop ()
    (flush-output)
    (match (lex-lcd-input (current-input-port) #:utf-8? utf-8?)
      [(? eof-object?) (void)]
      [(== (key #\D (set 'control))) (void)]
      [key
       (printf "Key: ~v\r\n" key)
       (loop)])))

(main)
