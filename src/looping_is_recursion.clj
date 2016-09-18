(ns looping-is-recursion)

(defn power [n k]
  (if (zero? k)
    1
  (let [helper (fn [n factor k]
                 (if (zero? k)
                   n
                   (recur (* n factor) factor (dec k))))]
    (helper n n (dec k)))))

(defn last-element [a-seq]
  (if (next a-seq)
          (recur (next a-seq))
          (first a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
    (or
     (not(= (first seq1) (first seq2)))
     (and (empty? seq1) (not (empty? seq2)))
     (and (empty? seq2) (not (empty? seq1)))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [sequ a-seq
        n 0]
    (cond
    (or (empty? a-seq) (= n (count a-seq))) nil
    (pred (first sequ)) n
     :else (recur (rest sequ) (inc n) ))))

(defn avg [a-seq]
  (loop [sum 0 n 0 i a-seq]
    (if (empty? i) (/ sum n)
      (recur (+ (first i) sum) (inc n) (rest i)))))

(defn parity [a-seq]
   (loop [[firstElem & elems] a-seq result #{}]
     (let [result (if (contains? result firstElem)
                      (disj result firstElem)
                      (conj result firstElem))]
       (if (empty? elems)
              result
              (recur elems result)))))

(defn fast-fibo [n]
  (loop [fibo 0 nextFibo 1 cnt n]
    (if (= 0 cnt)
      fibo
      (recur nextFibo (+ fibo nextFibo) (dec cnt)))))


(defn cut-at-repetition [a-seq]
   (loop [elems a-seq result [] tempSet #{}]
       (if (or (empty? elems)
               (contains? tempSet (first elems)))
                result
                (recur (rest elems) (conj result (first elems)) (conj tempSet (first elems))))))
