; The following is an adaptation of the fast-prime? procedure
; presented in section 1.2.6.

; To run, I recommend using leiningen (https://leiningen.org/)
; Run `lein repl` and execute `(load-file "prime.clj")`

(defn square [x] (* x x))

; note that cond's are a little bit different in clojure (less parens)
(defn expmod [base exp m]
  "Computes base^exp mod m"
  (cond
    (= exp 0) 1
    (even? exp)
      (rem
        (square (expmod base (/ exp 2) m))
        m)
    :else
      (rem
        (* base (expmod base (- exp 1) m))
        m)))
      
(defn random [n]
  "Returns a random nonnegative integer less than the input"
  (int (* n (Math/random))))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defn fast-prime? [n k]
  "Tests whether n is a probable prime by
  running the Fermat test k times"
  (cond
    (= k 0) true
    (fermat-test n) (fast-prime? n (- k 1))
    :else false))