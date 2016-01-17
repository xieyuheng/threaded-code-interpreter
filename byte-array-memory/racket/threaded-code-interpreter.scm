#lang racket
(require rnrs/bytevectors-6)

(define cell 4) ;; unit byte

(define memory:size (* 1024 1024))
(define memory (make-bytevector memory:size 0))

(define memory:get
  (lambda (index)
    (bytevector-uint-ref memory index (native-endianness) cell)))

(define memory:set
  (lambda (index value)
    (bytevector-uint-set! memory index value (native-endianness) cell)))

(define memory:current-free-address 0)

(define allocate-memory
  (lambda (size)
    (let ([return-address memory:current-free-address])
      (set! memory:current-free-address
            (+ memory:current-free-address size))
      return-address)))

(let ()
  (allocate-memory (* cell 64)) ;; underflow
  (void))

(define argument-stack:address (allocate-memory (* cell 1024)))
(define argument-stack:current-free-address argument-stack:address)

(define argument-stack:push
  (lambda (value)
    (memory:set argument-stack:current-free-address value)
    (set! argument-stack:current-free-address
          (+ argument-stack:current-free-address cell))))

(define argument-stack:pop
  (lambda ()
    (set! argument-stack:current-free-address
          (- argument-stack:current-free-address cell))
    (memory:get argument-stack:current-free-address)))

(let ()
  (allocate-memory (* cell 64)) ;; underflow
  (void))

(define return-stack:address (allocate-memory (* cell 1024)))
(define return-stack:current-free-address return-stack:address)

(define return-stack:push
  (lambda (value)
    (memory:set return-stack:current-free-address value)
    (set! return-stack:current-free-address
          (+ return-stack:current-free-address cell))))

(define return-stack:pop
  (lambda ()
    (set! return-stack:current-free-address
          (- return-stack:current-free-address cell))
    (memory:get return-stack:current-free-address)))

(define primitive-function-record:size (* 1024))
(define primitive-function-record (make-vector primitive-function-record:size 0))

(define primitive-function-counter 0)

(define primitive-function-record:get
  (lambda (index)
    (vector-ref primitive-function-record
                index)))

(define primitive-function-record:set
  (lambda (index function)
    (vector-set! primitive-function-record
                 index
                 function)))

(define create-primitive-function
  (lambda (function)
    (let ([return-address primitive-function-counter])
      (primitive-function-record:set primitive-function-counter
                                     function)
      (set! primitive-function-counter
            (+ primitive-function-counter 1))
      return-address)))

(define next:explainer-argument 0)

(define next
  (lambda ()
    (let* ([function-body (return-stack:pop)]
           [next-function-body (+ function-body cell)]
           [explainer (memory:get (memory:get function-body))])
      (return-stack:push next-function-body)
      (set! next:explainer-argument
            (+ (memory:get function-body) cell))
      ((primitive-function-record:get explainer)))))

(define in-host-name-record (make-hasheq))

(define data
  (lambda (value)
    (memory:set memory:current-free-address value)
    (set! memory:current-free-address
          (+ memory:current-free-address cell))))

(define mark
  (lambda (name-string)
    (hash-set! in-host-name-record
               name-string
               memory:current-free-address)))

(define link 0)

(define primitive-function-explainer
  (create-primitive-function
   (lambda ()
     ((primitive-function-record:get (memory:get next:explainer-argument))))))

(define define-primitive-function
  (lambda (name-string function)
    (let ([function-index (create-primitive-function function)])
      (data link)
      (set! link (- memory:current-free-address cell))
      (mark name-string)
      (data primitive-function-explainer)
      (data function-index))))

(define function-explainer
  (create-primitive-function
   (lambda ()
     (return-stack:push next:explainer-argument)
     (next))))

(define define-function
  (lambda (name-string . function-name-string-list)
    (data link)
    (set! link (- memory:current-free-address cell))
    (mark name-string)
    (data function-explainer)
    (map (lambda (function-name-string)
           (data (hash-ref in-host-name-record
                           function-name-string)))
         function-name-string-list)
    (void)))

(define variable-explainer
  (create-primitive-function
   (lambda ()
     (argument-stack:push (memory:get next:explainer-argument))
     (next))))

(define define-variable
  (lambda (name-string value)
    (data link)
    (set! link (- memory:current-free-address cell))
    (mark name-string)
    (data variable-explainer)
    (data value)))

(define-primitive-function "end"
  (lambda ()
    (return-stack:pop)
    (next)))

(define-primitive-function "bye"
  (lambda ()
    (display "bye bye ^-^/")
    (newline)))

(define-primitive-function "dup"
  (lambda ()
    (let* ([a (argument-stack:pop)])
      (argument-stack:push a)
      (argument-stack:push a)
      (next))))

(define-primitive-function "mul"
  (lambda ()
    (let* ([a (argument-stack:pop)]
           [b (argument-stack:pop)])
      (argument-stack:push (* a b))
      (next))))

(define-primitive-function "simple-wirte"
  (lambda ()
    (let* ([a (argument-stack:pop)])
      (display a)
      (newline)
      (next))))

(define-variable "little-test-number" 4)

(define-function "square"
  "dup"
  "mul"
  "end")

(define-function "little-test"
  "little-test-number"
  "square"
  "simple-wirte"
  "bye")

(define-function "first-function"
  "little-test"
  "end")

(define function-body-for-little-test
  (+ (hash-ref in-host-name-record
               "first-function")
     cell))

(define begin-to-interpret-threaded-code
  (lambda ()
    (return-stack:push function-body-for-little-test)
    (next)))

(begin-to-interpret-threaded-code)
