(ns interpreter.buffer)

(defmacro buffer-set [buffer index value]
  `(aset ~buffer (+ 1 ~index) ~value))

(defmacro buffer-get [buffer index]
  `(aget ~buffer (+ 1 ~index)))

(defn make-buffer [size init-value]
  (let [buffer (object-array (+ size 1))]
    (aset buffer 0 0)
    (loop [counter 0]
      (when (< counter size)
        ;; (aset buffer (+ 1 counter) init-value)
        (buffer-set buffer counter init-value)
        (recur (+ 1 counter))))
    buffer))

(defn buffer-get-cursor [buffer]
  (aget buffer 0))

(defn buffer-add-cursor [buffer value]
  (aset buffer 0
        (+ (buffer-get-cursor buffer) value)))

(defn buffer-allocate [buffer size]
  (let [return-address (swap! buffer buffer-get-cursor)]
    (buffer-add-cursor buffer size)
    return-address))
