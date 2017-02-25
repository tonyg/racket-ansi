#lang racket/base

(require make/setup-extension)

(provide pre-installer)

(define (pre-installer collections-top-path collection-path)
  (pre-install collection-path
	       (build-path collection-path "private")
	       "tty-raw-extension.c"
	       "."
	       '()
	       '()
	       '()
	       '()
	       '()
	       '()
	       (lambda (thunk) (thunk))
	       #t))
