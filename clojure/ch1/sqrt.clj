; The following is an adaptation of the sqrt function
; presented in section 1.1.7.

; To run, I recommend using leiningen (https://leiningen.org/)
; Run `lein repl` and execute `(load-file "sqrt.clj")`

; we can call java functions easily
; here the function abs calls the java method Math.abs
(defn abs [x]
    (Math/abs x))

; Function definition syntax is a bit different than scheme's
(defn square [x]
    (* x x))

(defn average [x y]
    (/ (+ x y) 2))

; let is a special form in Clojure
(defn good-enough? [guess x]
    (let [delta (abs (- x (square guess)))
          tolerance 0.0001]
        (< delta tolerance)))

(defn improve [guess x]
    (average guess (/ x guess)))

(defn sqrt-iter [guess x]
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
    (sqrt-iter 1.0 x))