(ns interpreter.stack
  (:use interpreter.buffer))

(def make-stack make-buffer)

(defn stack-push [stack value]
  (buffer-set stack
              (buffer-get-cursor stack)
              value)
  (buffer-add-cursor stack 1))

(defn stack-pop [stack]
  (buffer-add-cursor stack -1)
  (buffer-get stack
              (buffer-get-cursor stack)))
