(ns azra.core
  (:gen-class))

(defn -main
  []
  (println "Hello, World!"))


(defn sambut [nama] 
  (str "Hello, " nama "!" ))

(defn square [x] 
  (* x x))

(defn abc [a b c] 
  (let [disc (Math/sqrt (- (square b) (* 4 a c)))]
    [(/ (+ (- b) disc ) (* 2 a)) (/ (- (- b) disc) (* 2 a))])) 

(defn faktorial [x]
  (if (> x 1) 
    (* x (faktorial (dec x)))
     1))

(defn sum [lst]
  (if (empty? lst) 0 
    (+ (first lst) (sum (rest lst)))))

(defn product [x]
  (if (empty? x) 
    1
    (* (first x) (product (rest x)))))
    
(defn length [x]
  (if (empty? x) 
    0
    (+ 1 (length (rest x)))))

(defn pangkat [x y] 
  (if (< y 0)
    (/ 1 (* x (pangkat x (dec (- y)))))
    (if (= y 0)
    1
    (* x (pangkat x (dec y))))))

(defn my-reverse [ns] 
  (if (empty? ns)
    '()
    (conj (my-reverse (butlast ns)) (last ns))))

(defn prima [x y]
  (cond (= x  y) true
    (zero? (rem x y)) false
    :else (recur x (inc y))))



(defn prime? [x]
  (cond (<= x 1) false
    (= x 2) true
    (even? x) false
    :else (prima x 3)))

(defn listprima [x] 
  (filter 
    prime? (range (+ 2 x))))

(defn factor [x] 
  (filter #(zero? (rem x %)) (range 1 x)))

(apply + ((fn factor [x] 
  (filter #(zero? (rem x %)) (range 1 x))) 2))

(def pro-6-a 
    (sum (map #(pangkat % 2) (range 101))))

(def pro-6-b (pangkat (sum (range 101)) 2))


(defn pro-3 [x y]
  (cond (= x (first y)) x
    (= 0 (rem x (first y))) (pro-3 (quot x (first y)) (rest y))
         :else (pro-3 x (rest y))))

(defn pro-7 [x]
 (nth (listprima 100000) x))

(defn pro-8 [x] 
  (map read-string (map str (seq (str x)))))

(defn pro-21-a [x]
  (reduce + (- x) (factor x)))

(defn ami [x] (sum (butlast (factor x))))

(defn amicable? [x]
  (let [ami (sum (butlast (factor x))) 
        ami2 (sum (butlast (factor ami)))]
    (cond (= ami ami2) false 
      (= ami2 x) true )))

(def pro-21 (filter amicable? (range 10000)))



;(1 4 6 8 9 10 12 14 15 16 18 20) 2025


(defn pro-22 [x y]
  (cond 
    (= nil y) 
    x
    (= (rem x (first y)) 0) 
    (pro-22 x (first y))))

(defn angka20 []
  '(1 4 6 8 9 10 12 14 15 16 18 19 20))  


(defn diagonal [n]
  (let [a 8] (+ n a (diagonal a))))  

(defn pro-9 [lim]
  (for [a (range 3 lim)
        b (range a lim)
        c (range b lim)
        :let [x (* a a)
              y (* b b)
              z (* c c)]
        :when (and (== lim (+ a b c))
                   (== z (+ x y)))]
    (* a b c)))
        
(def pro-39
  (for [a (range 3 1000)
        b (range a 1000)
        c (range b 1000)
        :let [x (* a a)
              y (* b b)
              z (* c c)]
        :when (and (< 1000 (+ a b c) (= z (+ x y))))]
    (+ a b c)))

(defn alklo-testloop []
  (loop [x 5 result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result)))

(def alklo-sial 
  not=)
(def alklo-dalempos 
  #(butlast ( interleave %2 (take (count %2) (repeat %1)))))

(def alklo-bala (fn bala [x y] 
  (if (contains? y x)
  (= nil (x y))
  false)))

(def alklo-zipmaptai #(apply hash-map 
                             (mapcat list %2 (replicate (count %2) %1))))

;(def nama (read-string (str "[" (slurp "euler-22.txt") "]")) )


(def alklo-second2last (fn [x] (last (butlast x))))
(defn maxi [& xs]
  (if (every? #(> (first xs) %) (rest xs))
    (first xs)
    (apply maxi (rest xs))))

(def alklo-mylast (fn [x] (first (reverse x))))

(def alklo-countanjing (fn anjing [x] (if (empty? x) 
          0
          (+ 1 (anjing (rest x))))))


(def alklo-copysebnyk2 #(sort (apply concat (take 2 (repeat %)))))

(def alklo-copysebanyakbla #(apply concat (map (partial replicate %2 ) % )))

(def alklo-poseselangseling! #(butlast ( interleave %2 (take (count %2) (repeat %1)))))

(def alklo-palindromdetect #(= (seq %) (reverse %)))

(def alklo-figurethisoutpro-29 #(apply str (re-seq #"[A-Z]+" %)))


(defn alklo-fibo [x]
  (loop [awal 1 result [1] ]
    (if (= x (count result))
      result
      (recur (+' awal (last result)) (conj result awal)))))


;;buat masukin value kedalem list of keys

(def value '( #(zipmap %2 (take (count %2) (repeat %1)))))

(->> [1 2 3 4]
  (map #(replicate 2 %))
  (flatten))


(def alklo-interpose #(butlast ( interleave %2 (take (count %2) (repeat %1)))))

(def alklo-rangegokil #(take (- %2 %1) (iterate inc %)))

(partial remove #(= (nth %1 (- %2 1)) %1))

(def alklo-wow #(apply concat (partition (- %2 1) %2 %1)))

(def alklo-dotproduct #(apply + (map * %1 %2)))


(fn [x y]
  (loop [first 1 result []]
    (if x result
      (recur (y first) (conj [] first)))))

(def al-klojure80 (fn [a] (if (= a (apply + ((fn factor [x] (filter #(zero? (rem x %)) (range 1 x))) a))) true false)))







