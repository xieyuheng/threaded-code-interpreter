(ns interpreter.core
  (:use interpreter.buffer)
  (:use interpreter.stack))

(def cell-area (make-buffer (* 1024 1024) 0))

(def argument-stack (make-stack 1024 0))
(def return-stack (make-stack 1024 0))

(def primitive-function-record (make-buffer 1024 0))
(defn create-primitive-function [function]
  (let [cursor (buffer-get-cursor primitive-function-record)]
    (buffer-set primitive-function-record
                cursor
                function)
    (buffer-add-cursor primitive-function-record 1)
    cursor))

(def interpreter:explainer-argument (atom 0))
(defn interpreter []
  (try
    (loop []
      (let [jojo (stack-pop return-stack)
            jo (buffer-get cell-area jojo)
            explainer (buffer-get cell-area jo)]
        (stack-push return-stack (+ 1 jojo))
        (swap! interpreter:explainer-argument (fn [x] (+ 1 jo)))
        ((buffer-get primitive-function-record explainer)))
      (recur))
    (catch Exception exception
      (let [message-string (.getMessage exception)]
        (cond
          (= message-string "exit") (str "exit")
          :else message-string)))))

(def name-jo-map (atom (hash-map)))

(defn data [value]
  (buffer-set cell-area
              (buffer-get-cursor cell-area)
              value)
  (buffer-add-cursor cell-area 1))

(defn mark [name-string]
  (swap! name-jo-map
         assoc name-string (buffer-get-cursor cell-area)))


(def primitive-function-explainer
  (create-primitive-function
   (fn []
     ((buffer-get primitive-function-record
                  (buffer-get cell-area
                              @interpreter:explainer-argument))))))

(defn define-primitive-function [name-string function]
  (let [function-index (create-primitive-function function)]
    (mark name-string)
    (data primitive-function-explainer)
    (data function-index)))

(def function-explainer
  (create-primitive-function
   (fn []
     (stack-push return-stack @interpreter:explainer-argument))))

(defn define-function [name-string & function-name-string-list]
  (mark name-string)
  (data function-explainer)
  (mapv #(data (@name-jo-map %1))
        function-name-string-list))

(def variable-explainer
  (create-primitive-function
   (fn []
     (stack-push argument-stack
                 (buffer-get cell-area
                             @interpreter:explainer-argument)))))

(defn define-variable [name-string value]
  (mark name-string)
  (data variable-explainer)
  (data value))


(define-primitive-function "end"
  (fn []
    (stack-pop return-stack)))

(define-primitive-function "bye"
  (fn []
    (print "bye bye ^-^/")
    (newline)
    (throw (Exception. "exit"))))

(define-primitive-function "dup"
  (fn []
    (let [a (stack-pop argument-stack)]
      (stack-push argument-stack a)
      (stack-push argument-stack a))))

(define-primitive-function "mul"
  (fn []
    (let [a (stack-pop argument-stack)
          b (stack-pop argument-stack)]
      (stack-push argument-stack (* a b)))))

(define-primitive-function "simple-wirte"
  (fn []
    (let [a (stack-pop argument-stack)]
      (print a)
      (newline))))

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

(defn -main [& args]
  (stack-push return-stack (+ 1 (@name-jo-map "first-function")))
  (interpreter))
