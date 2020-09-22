#lang racket/base

(require rktermios)

(provide tty-raw! tty-restore!)

(define rktermios-saved #f)

(define (tty-raw!)
  (set! rktermios-saved (rktermios-get))
  (define tios-raw (rktermios-copy rktermios-saved))
  (rktermios-make-raw! tios-raw)
  (rktermios-load tios-raw)
  (file-stream-buffer-mode (current-output-port) 'none))

(define (tty-restore!)
  (when rktermios-saved
    (rktermios-load rktermios-saved)
    (set! rktermios-saved #t)))
