#lang racket/base

(require "main.rkt")

(define (display* . things)
  (for-each display things)
  (flush-output))

(define (main)
  (tty-raw!)

  (display* (dec-soft-terminal-reset)
            (device-request-screen-size))

  (define report (lex-lcd-input (current-input-port)))
  (printf "The reported screen size is ~a columns and ~a rows.\r\n"
          (screen-size-report-columns report)
          (screen-size-report-rows report))
  (printf "The raw report value itself is ~v.\r\n" report)

  (display* (dec-soft-terminal-reset)))

(module+ main
  (main))
