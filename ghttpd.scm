#! gosh
;; -*- coding: utf-8 -*-

(define-module ghttpd (export ghttpd-start-server))
(select-module ghttpd)

(use gauche.fcntl)
(use gauche.interactive)
(use gauche.net)
(use gauche.selector)

(define (ghttpd-process-request sock document-root)
  (let* (
      (read-port (socket-input-port sock)) 
      (write-port (socket-output-port sock)))
    (guard (e (else
        (display "HTTP/1.0 404 Not Found\r\nConnection: close\r\n\r\n" 
            write-port)))
      (let* (
          (columns (string-split (read-line read-port) char-whitespace?))
          (abs-path (string-join (list document-root (car (cdr columns))) "")))
        (define (read-request port)
          (let ((line (read-line port)))
            (if (or (equal? line "") (eof-object? line))
              #t
              (read-request port))))
        (read-request read-port)

        (display "HTTP/1.0 200 OK\r\nConnection: close\r\n\r\n" write-port)
        (copy-port (open-input-file abs-path) write-port)))
    (close-output-port write-port)
    (close-input-port read-port)))

(define (ghttpd-start-server port document-root)
  (let ((sock (make-server-socket 'inet port :reuse-addr? #t)) (terminated #f))
    (define (mainloop sock)
      (let ((selector (make <selector>)))
        (define (accept-client fd flags)
          (let accept ((sock sock))
            (guard (e
              ((<system-error> e) 
                (let ((errno (ref e 'errno))) 
                  (if (or (equal? errno EAGAIN) (equal? errno ENETDOWN) (equal? errno EPROTO) (equal? errno ENOPROTOOPT) (equal? errno EHOSTDOWN) (equal? errno ENONET) (equal? errno EHOSTUNREACH) (equal? errno EOPNOTSUPP) (equal? errno ENETUNREACH))
                    (accept sock)
                    (raise e)))))
                (let ((client-sock (socket-accept sock)))
                  (if (equal? (class-of client-sock) <socket>)
                    (begin
                      (ghttpd-process-request client-sock document-root)
                      (socket-close client-sock))
                    (accept sock))))))

        (selector-add! selector (socket-fd sock) accept-client '(r))
        (selector-select selector 1000000)
        (if (not terminated)
          (mainloop sock)
          #t)))

    (let ((signal-handler (lambda (sig) (set! terminated #t))))
      (map (cut set-signal-handler! <> signal-handler) (list SIGINT SIGTERM)))
    (set-signal-handler! SIGPIPE #f)
    (let ((fd (socket-fd sock)))
      (sys-fcntl fd F_SETFD (logior O_NONBLOCK (sys-fcntl fd F_GETFD))))
    (mainloop sock)
    (socket-close sock)))

(provide "ghttpd")

;; vim: tabstop=2 shiftwidth=2 expandtab softtabstop=2 filetype=scheme
