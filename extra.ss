;; hello extra runtime
(define (eval-str s) (with-input-from-string s (lambda () (eval (read)))))
(define (eval-str2 ty s)
  (display "\033[0;91m")
  (display s)
  (display "\033[0m\n")
  (with-input-from-string s (lambda () (eval (read)))))
(define (call-ffi t cmd args)
  (display cmd)
  (newline)
  (display args)
  (newline)
  (apply (eval-str cmd) args))
(define (list-cons t a l) (cons a l))
(define (bytevector-contents bv idx) (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 idx)))

;(define emptyArgs (list))
;(define (addArg v l) '())
;%foreign "scheme:addArg"
;addArg : String -> a -> ArgList -> ArgList
; prim_callFFI : {a : Type} -> String -> ArgList -> PrimIO a

;(load-shared-object "libm.so.6")
;(display ((foreign-procedure #f "sin" (double-float) double-float) 1.0))

;(display "\n")

;(define x (list 1.0))
;(display (apply (eval-str "(foreign-procedure #f \"sin\" (double-float) double-float)") x) )

;(display "\n")

;(display (call-ffi #f "(foreign-procedure #f \"sin\" (double-float) double-float)" (list 1.0)))

(define c-memcpy
  (foreign-procedure "memcpy" (uptr scheme-object size_t) uptr))

(define (string->foreign-buffer2 str)
  (let* ([bv (string->utf8 str)]            ; 2. Átalakítjuk a stringet UTF-8 bytevektorrá
         [len (bytevector-length bv)]       ; 3. Lekérjük a hosszát
         ;; 4. Lefoglalunk len + 1 byte-ot a külső memóriában (a +1 a C-s lezáró null-nak kell: \0)
         [addr (foreign-alloc (+ len 1))])  
    
    ;; 5. Kimásoljuk a bytevektor tartalmát a lefoglalt címre
    ;; A c-memcpy-nek átadhatjuk közvetlenül a 'bv' Scheme-objektumot, az FFI kezeli az adatcímét.
    (c-memcpy addr bv len)
    
    ;; 6. Manuálisan lezárjuk a puffert egy 0-s byte-tal a végén (null-terminated string a C-nek)
    ;; A 'foreign-set!' segítségével közvetlenül írhatunk a külső memóriacímre
    (foreign-set! 'unsigned-8 addr len 0)
    
    ;; Visszaadjuk a lefoglalt puffer kezdőcímét (uptr)
    addr))

(define (string->foreign-buffer str)
  (let* ([bv (bytevector (string->list str))]            ; 2. Átalakítjuk a stringet UTF-8 bytevektorrá
         [len (bytevector-length bv)]       ; 3. Lekérjük a hosszát
         ;; 4. Lefoglalunk len + 1 byte-ot a külső memóriában (a +1 a C-s lezáró null-nak kell: \0)
         [addr (foreign-alloc (+ len 1))])  
    
    ;; 5. Kimásoljuk a bytevektor tartalmát a lefoglalt címre
    ;; A c-memcpy-nek átadhatjuk közvetlenül a 'bv' Scheme-objektumot, az FFI kezeli az adatcímét.
    (c-memcpy addr bv len)
    
    ;; 6. Manuálisan lezárjuk a puffert egy 0-s byte-tal a végén (null-terminated string a C-nek)
    ;; A 'foreign-set!' segítségével közvetlenül írhatunk a külső memóriacímre
    (foreign-set! 'unsigned-8 addr len 0)
    
    ;; Visszaadjuk a lefoglalt puffer kezdőcímét (uptr)
    addr))
