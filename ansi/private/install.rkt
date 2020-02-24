#lang racket/base

(require dynext/file
         dynext/link
         racket/file)

(provide pre-installer)

;; Used by "../info.rkt" (so this-collection-path is "..").

;; Heavily based on Sam Tobin-Hochstadt's bcrypt/private/install.rkt
;; https://github.com/samth/bcrypt.rkt

(define (pre-installer collections-top-path this-collection-path)
  (define ansi/private/
    (build-path this-collection-path "private"))
  (parameterize ([current-directory ansi/private/]
                 [current-use-mzdyn #f])
    (define tty_raw.c
      (build-path ansi/private/ "tty_raw.c"))
    (define libtty_raw.so
      (build-path ansi/private/
                  "compiled"
                  "native"
                  (system-library-subpath #f)
                  (append-extension-suffix "libtty_raw")))
    (when (file-exists? libtty_raw.so)
      (delete-file libtty_raw.so))
    (make-parent-directory* libtty_raw.so)
    (link-extension #f ;; not quiet
                    (list tty_raw.c)
                    libtty_raw.so)))
