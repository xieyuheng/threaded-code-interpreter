#lang racket

(define cell-area:size (* 1024 1024))
(define cell-area (make-vector cell-area:size 0))

(define cell-area:get
  (lambda (index)
    (vector-ref cell-area index)))

(define cell-area:set
  (lambda (index value)
    (vector-set! cell-area index value)))

(define cell-area:pointer 0)

(define allocate-cell-area
  (lambda (size)
    (let ([return-address cell-area:pointer])
      (set! cell-area:pointer
            (+ cell-area:pointer size))
      return-address)))

(define argument-stack (make-vector 1024))
(define argument-stack:pointer 0)

(define argument-stack:push
  (lambda (value)
    (vector-set! argument-stack
                 argument-stack:pointer value)
    (set! argument-stack:pointer
          (+ argument-stack:pointer 1))))

(define argument-stack:pop
  (lambda ()
    (set! argument-stack:pointer
          (- argument-stack:pointer 1))
    (vector-ref argument-stack
                argument-stack:pointer)))

(define return-stack (make-vector 1024))
(define return-stack:pointer 0)

(define return-stack:push
  (lambda (value)
    (vector-set! return-stack
                 return-stack:pointer value)
    (set! return-stack:pointer
          (+ return-stack:pointer 1))))

(define return-stack:pop
  (lambda ()
    (set! return-stack:pointer
          (- return-stack:pointer 1))
    (vector-ref return-stack
                return-stack:pointer)))

(define primitive-function-record:size 1024)
(define primitive-function-record
  (make-vector primitive-function-record:size 0))

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
    (let* ([jojo (return-stack:pop)]
           [jo (cell-area:get jojo)]
           [next-jojo (+ jojo 1)]
           [explainer (cell-area:get jo)])
      (return-stack:push next-jojo)
      (set! next:explainer-argument (+ jo 1))
      ((primitive-function-record:get explainer)))))

(define in-host-name-record (make-hasheq))

(define data
  (lambda (value)
    (cell-area:set cell-area:pointer value)
    (set! cell-area:pointer
          (+ cell-area:pointer 1))))

(define mark
  (lambda (name-string)
    (hash-set! in-host-name-record
               name-string
               cell-area:pointer)))

(define link 0)

(define define-header
  (lambda (name-string explainer)
    (data link)
    (set! link (- cell-area:pointer 1))
    (mark name-string)
    (data explainer)))

(define primitive-function-explainer
  (create-primitive-function
   (lambda ()
     ((primitive-function-record:get
       (cell-area:get next:explainer-argument))))))

(define define-primitive-function
  (lambda (name-string function)
    (let ([function-index (create-primitive-function function)])
      (define-header name-string
        primitive-function-explainer)
      (data function-index))))

(define function-explainer
  (create-primitive-function
   (lambda ()
     (return-stack:push next:explainer-argument)
     (next))))

(define define-function
  (lambda (name-string . function-name-string-list)
    (define-header name-string
      function-explainer)
    (map (lambda (function-name-string)
           (data (hash-ref in-host-name-record
                           function-name-string)))
         function-name-string-list)
    (void)))

(define variable-explainer
  (create-primitive-function
   (lambda ()
     (argument-stack:push (cell-area:get next:explainer-argument))
     (next))))

(define define-variable
  (lambda (name-string value)
    (define-header name-string
      variable-explainer)
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
     1))

(define begin-to-interpret-threaded-code
  (lambda ()
    (return-stack:push function-body-for-little-test)
    (next)))

(begin-to-interpret-threaded-code)
