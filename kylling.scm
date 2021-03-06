(use tcp irregex ports srfi-13 srfi-14 srfi-69)

(foreign-declare
  "#include <sys/capsicum.h>")

(define cap_enter (foreign-lambda int "cap_enter"))

(define-record kylling server port channel transport nick in out)

(define-record-printer (kylling k out)
  (fprintf out "#,(kylling server:~a port:~a channel:~a transport:~a nick:~a)"
           (kylling-server k) (kylling-port k) (kylling-channel k)
           (kylling-transport k) (kylling-nick k)))

(define (make-default-kylling)
  (make-kylling "irc.efnet.org" "6667" "#scmc_" "plain" "kylling" #f #f))

(define (prefix->nick prefix)
  (cond
    ((irregex-match "([^!]+)!.*" prefix) =>
      (lambda (m)
        (irregex-match-substring m 1)))
    (#t (error "invalid prefix:" prefix))))

(define (channel? str)
  (or (string-prefix? "#" str) (string-prefix? "&" str)))

(define (strip-chars str chars)
  (string-delete (->char-set chars) str))

(define (privmsg k dst msg)
  (if (string? msg)
    (format (kylling-out k) "PRIVMSG ~a :~a\r\n" dst
            (strip-chars msg "\r\n\t"))
    (privmsg k dst (format #f "~a" msg))))


(define (exn-message e)
  ((condition-property-accessor 'exn 'message "unknown error") e))

(define (eval-cmd k dst str)
  (with-input-from-string str
    (lambda ()
      (condition-case
        (privmsg k dst (eval (read)))
        [var () (privmsg k dst (exn-message var))]))))

(define cmd-table (make-hash-table))

(define (define-cmd k dst rest)
  (cond
    ((irregex-match "([^ ]+) +(.*)" rest) =>
      (lambda (m)
        (with-input-from-string (irregex-match-substring m 2)
          (lambda ()
            (condition-case
              (hash-table-set! cmd-table (irregex-match-substring m 1)
                               (eval (read)))
              [var () (privmsg k dst (exn-message var))])))))))

(define (process-privmsg* k dst msg)
  (cond
    ((irregex-match "!define +(.*)" msg) =>
      (lambda (m)
        (define-cmd k dst (irregex-match-substring m 1))))
    ((irregex-match "!([^ ]+) *(.*)" msg) =>
      (lambda (m)
        (if (hash-table-exists? cmd-table (irregex-match-substring m 1))
          (condition-case
            (let
              ([val (hash-table-ref cmd-table (irregex-match-substring m 1))])
              (if (procedure? val)
                (val k dst (irregex-match-substring m 2))
                (privmsg k dst val)))
            [var () (privmsg k dst (exn-message var))])
          (privmsg k dst "no such symbol"))))))

(define (process-privmsg k prefix params)
  (cond
    ((irregex-match " ([^ ]+) :(.*)" params) =>
      (lambda (m)
        (let
          ([dst (irregex-match-substring m 1)]
           [msg (irregex-match-substring m 2)])
          (if (channel? dst)
            (process-privmsg* k dst msg)
            (process-privmsg* k (prefix->nick prefix) msg)))))))

(define (process-line k line)
  (print line)
  (cond
    ((irregex-match "(:([^ ]+) )?([^ ]+)(.*)?\r?\n?" line) =>
      (lambda (m)
        (let ([prefix (irregex-match-substring m 2)]
              [cmd (irregex-match-substring m 3)]
              [params (irregex-match-substring m 4)])
          (cond
            [(equal? cmd "PING") (format (kylling-out k) "PONG~a\r\n" params)]
            [(equal? cmd "001")
             (format (kylling-out k) "JOIN ~a\r\n" (kylling-channel k))]
            [(equal? cmd "PRIVMSG") (process-privmsg k prefix params)]
            ))))))

(define (main-loop k)
  (let-values
    ([(in out)
      (tcp-connect (kylling-server k) (string->number (kylling-port k)))])
    (cap_enter)
    (kylling-in-set! k in)
    (kylling-out-set! k out)
    (format out "NICK ~a\r\n" (kylling-nick k))
    (format out "USER guest tolmoon tolsun :Ronnie Reagan\r\n")
    (let loop
      ([line (read-line in)])
      (if (eof-object? line)
        line
        (begin
          (process-line k line)
          (loop (read-line in)))))))

(define (parse-opts kylling opts)
  (if (null? opts)
    kylling
    (let ([opt (car opts)])
      (cond
        [(equal? opt "-server") (kylling-server-set! kylling (cadr opts))]
        [(equal? opt "-port") (kylling-port-set! kylling (cadr opts))]
        [(equal? opt "-channel") (kylling-channel-set! kylling (cadr opts))]
        [(equal? opt "-transport") (kylling-transport-set! kylling (cadr opts))]
        [(equal? opt "-nick") (kylling-nick-set! kylling (cadr opts))]
        [#t (error "invalid option:" opt)])
      (parse-opts kylling (cddr opts)))))


(tcp-read-timeout #f)
(tcp-connect-timeout 30000)
(let
  ([k (make-default-kylling)])
  (print (parse-opts k (command-line-arguments)))
  (main-loop k))
