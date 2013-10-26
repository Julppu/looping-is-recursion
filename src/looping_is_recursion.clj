(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp) 1 (* base (power base (- exp 1)))))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [seq1-first (first seq1)
        seq2-first (first seq2)
        seq1-empty (empty? seq1)
        seq2-empty (empty? seq2)]
    (cond
      (and seq1-empty seq2-empty) true
      (or seq1-empty seq2-empty) false
      (not= seq1-first seq2-first) false
      :else (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) ind
      :else (recur (inc ind) (rest s)))))

(defn avg [a-seq]
  (loop [acc (first a-seq)
         c 1
         s (rest a-seq)]
    (if (empty? s) (/ acc c) (recur (+ acc (first s)) (inc c) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
        (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [t #{} a a-seq]
      (if (empty? a) t (recur (toggle t (first a)) (rest a))))))

(defn fast-fibo [n]
  (case n 0 0 1 1
    (loop [prev 1
           acc 1
           i 2]
      (if (= i n) acc (recur acc (+ prev acc) (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         prev-vals #{}
         s a-seq]
    (if (or (contains? prev-vals (first s)) (empty? s)) acc
      (recur (conj acc (first s)) (conj prev-vals (first s)) (rest s)))))

;_;
