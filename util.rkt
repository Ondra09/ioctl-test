#lang racket

(require racket/udp)
(require ffi/unsafe
         ffi/unsafe/define)
(require racket/list racket/port racket/system racket/string)
(provide (all-defined-out))


(define-ffi-definer define-pty (ffi-lib "libutil"))

#|
;; SIOCGIFCONF

(define scheme_get_port_file_descriptor
  (get-ffi-obj "scheme_get_port_file_descriptor" #f
               (_fun (port : _scheme) (fd : (_ptr o _int))
                     -> (ret : _int)
                     -> (if (ret . < . 0)
                            (error "couldn't get port file descriptor")
                            fd))))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define (testCall fd)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     (gid : (_ptr o _int))
                     -> (ret : _int)
                     -> (if (< ret 0) (error "ioctl failed")
                          gid)))
  (ioctl fd TIOCGPGRP))


(define (get-if-conf fd status)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     (status : (_ptr io _ifconf))
                     -> (ret : _int)
                     -> (if (< ret 0) (error (string-append "ioctl failed err: " (number->string ret)))
                          status)))
  (ioctl fd SIOCGIFCONF status))
|#

;                                                                                                                                              
;                                                                                                                                              
;                                                                                                                                              
;     ;;;;                        ;                                                                                                            
;    ;   ;;                       ;                     ;                                                                               ;      
;   ;                             ;                     ;                                                                               ;      
;   ;           ;;;;      ;;;;    ;    ;      ;;;;    ;;;;;;                ;;;;;   ;      ;  ; ;;;;    ; ;;;;      ;;;;      ; ;;;   ;;;;;;   
;   ;          ;    ;    ;    ;   ;   ;      ;    ;     ;                 ;;     ;  ;      ;  ;;    ;   ;;    ;    ;    ;     ;;   ;    ;      
;    ;;;      ;      ;  ;         ;  ;      ;      ;    ;                 ;         ;      ;  ;      ;  ;      ;  ;      ;    ;         ;      
;      ;;;;   ;      ;  ;         ; ;       ;      ;    ;                 ;;        ;      ;  ;      ;  ;      ;  ;      ;    ;         ;      
;          ;  ;      ;  ;         ;; ;      ;;;;;;;;    ;                  ;;;;;;   ;      ;  ;      ;  ;      ;  ;      ;    ;         ;      
;          ;  ;      ;  ;         ;   ;     ;           ;                       ;;  ;      ;  ;      ;  ;      ;  ;      ;    ;         ;      
;   ;      ;  ;      ;  ;         ;    ;    ;           ;                        ;  ;      ;  ;      ;  ;      ;  ;      ;    ;         ;      
;   ;;    ;;   ;    ;    ;    ;   ;     ;    ;     ;    ;                 ;     ;;  ;;    ;;  ;;    ;   ;;    ;    ;    ;     ;         ;      
;    ;;;;;      ;;;;      ;;;;    ;      ;    ;;;;;      ;;;               ;;;;;     ;;;;; ;  ; ;;;;    ; ;;;;      ;;;;      ;          ;;;   
;                                                                                             ;         ;                                      
;                                                                                             ;         ;                                      
;                                                                                             ;         ;                                      
;                                                                                                                                              

(define AF_INET 2)
(define SOCK_DGRAM 2)
(define IPPROTO_IP 0)

(define (my-socket)
  (define-pty socket (_fun
                     (a : _int)
                     (b : _int)
                     (c : _int)
                     -> (ret : _int)
                     -> (if (< ret 0) (error (string-append "socket failed err: " (number->string ret)))
                          ret)))
  (socket AF_INET SOCK_DGRAM IPPROTO_IP))

(define (close-socket fd)
  (define-pty close (_fun
                     (a : _int)
                     -> (ret : _int)
                     -> (if (< ret 0) (error (string-append "close failed err: " (number->string ret)))
                          ret)))
  (close fd))

;; (define sock (udp-open-socket "127.0.0.1"))

;; (define a 0)
;; (testCall 1 a)

(define (extract-name ptr)
  (define (recurse counter)
  (if (= (array-ref ptr counter) 0)
      '()
      (cons (array-ref ptr counter) (recurse (+ counter 1)))
      ))
  (recurse 0))

(define (byte-array->bytes array)
  (let* ([len   (array-length array)]
         [byte* (make-bytes len)])
    (for ([i (in-range len)])
      (bytes-set! byte* i (array-ref array i)))
    byte*))


;                                                    
;                                                    
;                                                    
;    ;;;;;      ;;;;       ;;;;  ;;;;;;;;;  ;        
;      ;       ;    ;    ;;   ;;     ;      ;        
;      ;      ;;    ;;   ;           ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;      ;  ;            ;      ;        
;      ;      ;;    ;;   ;           ;      ;        
;      ;       ;    ;    ;;   ;;     ;      ;        
;    ;;;;;      ;;;;       ;;;;      ;      ;;;;;;;; 
;                                                    
;                                                    
;                                                    
;
; https://github.com/BartAdv/racket-serial/blob/master/ioctl.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define scheme_get_port_fd
  (get-ffi-obj 'scheme_get_port_fd #f (_fun _racket -> _intptr)))

(define _file-port/no-null
  (make-ctype _int
    (λ (x)
      (unless (port? x)
        (raise-argument-error '_file-port/no-null "file-stream-port" x))
      (scheme_get_port_fd x))
    (lambda (x) x)))
|#
(define strerror
  (get-ffi-obj 'strerror #f (_fun _int -> _bytes)))

(define (check v who)
  (unless (zero? v)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide get-ioctl-ffi)

(define-syntax (get-ioctl-ffi stx)
  (syntax-case stx ()
    [(_ type)
     #'(get-ffi-obj 'ioctl #f (_fun #:save-errno 'posix
				    _int
				    _int
				    (o : type)
				    -> (r : _int)
				    -> (when (check r 'ioctl) o)))]
    [(_)
     #'(get-ffi-obj 'ioctl #f (_fun #:save-errno 'posix
				    _int
				    _int
				    -> (r : _int)
				    -> (check r 'ioctl)))]))