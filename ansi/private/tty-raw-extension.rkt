#lang racket/base

(require ffi/unsafe
         ffi/unsafe/global
         ffi/unsafe/define)

(provide (protect-out tty-raw!
                      tty-restore!))

(define (local-lib-dirs)
  ;; FIXME: There's probably a better way to do this with
  ;; define-runtime-path and cross-system-library-subpath,
  ;; but this is what the bcrypt package is doing.
  (list (build-path (collection-path "ansi")
                    "private"
                    "compiled"
                    "native"
                    (system-library-subpath #f))))

(define libtty_raw
  (ffi-lib "libtty_raw" #:get-lib-dirs local-lib-dirs))

(define-ffi-definer define-tty libtty_raw
  #:default-make-fail make-not-available)

(define-tty tty-raw!
  (_fun #:in-original-place? #t
        -> _stdbool)
  #:c-id tty_raw)

(define-tty tty-restore!
  (_fun #:in-original-place? #t
        -> _stdbool)
  #:c-id tty_restore)

(unless (register-process-global #"ansi-private-tty-raw-has-set-restore-at-exit" #"")
  (define-tty tty_set_restore_at_exit
    (_fun -> _void))
  (tty_set_restore_at_exit))
