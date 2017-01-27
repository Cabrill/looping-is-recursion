(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b n]
                 (if (zero? n)
                   acc
                   (recur (* acc b) b (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [n sq]
                 (if (zero? n)
                   (first sq)
                   (recur (dec n) (rest sq))))]
    (if (empty? a-seq)
      nil
      (helper (dec (count a-seq)) a-seq))))


(defn seq= [seq1 seq2]
    (let [helper (fn [acc s1 s2]
                 (if (or (not acc) (and (empty? s1) (empty? s2)))
                   acc
                   (recur (= (first s1) (first s2)) (rest s1) (rest s2))))]
      (helper (= (count seq1) (count seq2)) seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         s a-seq
         p pred]
    (if (empty? s)
      nil
      (if (p (first s))
          idx
          (recur (inc idx) (rest s) p)))))

(defn avg [a-seq]
  (loop [ttl 0
         cnt 0
         sq a-seq]
    (if (empty? sq)
      (if (= 0 cnt)
        cnt
        (/ ttl cnt))
      (recur (+ ttl (int (first sq))) (inc cnt) (rest sq)))))

(defn parity [a-seq]
  (loop [ns #{}
         os a-seq]
    (if (empty? os)
      ns
    (if (contains? ns (first os))
      (recur (disj ns (first os)) (rest os))
      (recur (conj ns (first os)) (rest os))))))


(defn fast-fibo [n]
  (loop [acc 0
         ln 1
         at 0
         until n]
    (if (= at until)
      acc
      (recur (+ ln acc) acc (inc at) until))))

(defn cut-at-repetition [a-seq]
  (loop [ns []
         os a-seq]
    (if (or (empty? os) (contains? (set ns) (first os)))
      ns
      (recur (conj ns (first os)) (rest os)))))
