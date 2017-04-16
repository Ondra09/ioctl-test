#lang racket/base


(require ffi/unsafe
         ffi/unsafe/define)

(require "util.rkt")

(define-cstruct _sockaddr ([sa_family _ushort]
                           [sa_data (_array _byte 14)]
                           ))
(define IFNAMSIZ 16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define TIOCMGET #x5415)
(define TIOCGPGRP #x540F)
(define SIOCETHTOOL #x8946)
(define SIOCGIFCONF #x8912)

#|
struct ifmap {
                      unsigned long   mem_start;
                      unsigned long   mem_end;
                      unsigned short  base_addr;
                      unsigned char   irq;
                      unsigned char   dma;
                      unsigned char   port;
                  };
|#

(define-cstruct _ifmap ([mem_start _ulong]
                        [mem_end _ulong]
                        [base_addr _ushort]
                        [irq _ubyte]
                        [dma _ubyte]
                        [port _ubyte]
                        ))

#|
struct sockaddr {
    unsigned short    sa_family;    // address family, AF_xxx
    char              sa_data[14];  // 14 bytes of protocol address
};
struct ifreq {
               char ifr_name[IFNAMSIZ]; /* Interface name */
               union {
                   struct sockaddr ifr_addr;
                   struct sockaddr ifr_dstaddr;
                   struct sockaddr ifr_broadaddr;
                   struct sockaddr ifr_netmask;
                   struct sockaddr ifr_hwaddr;
                   short           ifr_flags;
                   int             ifr_ifindex;
                   int             ifr_metric;
                   int             ifr_mtu;
                   struct ifmap    ifr_map;
                   char            ifr_slave[IFNAMSIZ];
                   char            ifr_newname[IFNAMSIZ];
                   char           *ifr_data;
               };
           };
|#


(define-cstruct _ifreq ([ifr_name (_array _byte IFNAMSIZ)]
                        [myunion (_union _sockaddr                             
                                    _short
                                    _int
                                    _ifmap
                                    (_array _byte IFNAMSIZ)
                                    _pointer
                                    )]
                        ))
           

#|
struct ifconf {
                      int                 ifc_len; /* size of buffer */
                      union {
                          char           *ifc_buf; /* buffer address */
                          struct ifreq   *ifc_req; /* array of structures */
                      };
                  };
|#
(define ifconf-union-type
  (_union _pointer
          _int32))

(define-cstruct _ifconf ([ifc_len _int]                        
                         [myunion ifconf-union-type]
                         ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define data-type (_array _byte 1024))

(define data (malloc data-type))
(define ifconf-p (malloc _ifconf))
(define ifconf-d (ptr-ref ifconf-p _ifconf))

(set-ifconf-ifc_len! ifconf-d 1024)

(union-set! (ifconf-myunion ifconf-d) 0 data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-if-conf
  (let ([f (get-ioctl-ffi (_ptr io _ifconf))])
    (λ (port ifconf-d)
      (f port SIOCGIFCONF ifconf-d))))

(define sock (my-socket))
(define ifconf-res (get-if-conf sock ifconf-d))

(define ifreq-p (union-ref (ifconf-myunion ifconf-res) 0))
(define ifreq-d (ptr-ref (union-ref (ifconf-myunion ifconf-res) 0) _ifreq))

(ctype-sizeof _ifreq)
(display "ifreq 2 name\n")
(define ifreq-d2 (ptr-add ifreq-p (ctype-sizeof _ifreq)))
; (array-ref (ifreq-ifr_name (ptr-ref ifreq-d2 _ifreq)) 0)

(define ext-name (extract-name (ifreq-ifr_name (ptr-ref ifreq-d _ifreq))))
(define ext-name2 (extract-name (ifreq-ifr_name (ptr-ref ifreq-d2 _ifreq))))

(define b-name (list->bytes ext-name))
(define b-name2 (list->bytes ext-name2))
b-name
b-name2

(byte-array->bytes (ifreq-ifr_name (ptr-ref ifreq-d _ifreq)))
(byte-array->bytes (ifreq-ifr_name (ptr-ref ifreq-d2 _ifreq)))
;;(ifconf-ifc_len ifconf-res)

(close-socket sock) ;; 