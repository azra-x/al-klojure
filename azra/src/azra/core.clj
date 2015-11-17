(ns azra.core)

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
    (*' x (faktorial (dec x)))
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
    (/ 1 (*' x (pangkat x (dec (-' y)))))
    (if (= y 0)
    1
    (*' x (pangkat x (dec y))))))

(defn my-reverse [ns] 
  (if (empty? ns)
    '()
    (conj (my-reverse (butlast ns)) (last ns))))

(defn prima [x y]
  (loop [akarnya (Math/sqrt x)
         y y] 
    (cond (< akarnya  y) true
    (zero? (rem x y)) false
    :else (recur akarnya (+ 2 y)))))

(defn prime? [x]
  (cond (<= x 1) false
    (= x 2) true
    (even? x) false
    :else (prima x 3)))

(defn listprima [x y] 
  (filter 
    prime? (range x y)))

(defn factor [x] 
  (filter #(zero? (rem x %)) (range 1 (+' 1 x))))

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
        
(defn euler-39 [m]
  (for [a (range 3 500)
        b (range a 500)
        c (range b 500)
        :let [x (* a a)
              y (* b b)
              z (* c c)]
        :when (and (<= (+ a b c) m) (= z (+ x y)))]
    (list a b c)))

(defn alklo-testloop []
  (loop [x 5 result []]
    (if (> x 0)
      (recur (dec x) (conj result (+ 2 x)))
      result)))

(def alklo-sial not=)

(def alklo-dalempos #(butlast ( interleave %2 (take (count %2) (repeat %1)))))

(def alklo-bala (fn bala [x y] 
  (if (contains? y x)
  (= nil (x y))
  false)))

(def alklo-zipmaptai  #(apply hash-map (mapcat list %2 (replicate (count %2) %1))))

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


(def alklo-splittai #(vector (vec (take %1 %2)) (subvec %2 %1)))

(def alklo-interpose #(butlast ( interleave %2 (take (count %2) (repeat %1)))))

(def alklo-rangegokil #(take (- %2 %1) (iterate inc %)))

(partial remove #(= (nth %1 (- %2 1)) %1))

(def alklo-wow #(apply concat (partition (- %2 1) %2 %1)))

(def alklo-dotproduct #(apply + (map * %1 %2)))


(fn [x y]
  (loop [first 1 result []]
    (if x result
      (recur (y first) (conj [] first)))))

(def alklo-80 (fn [a] (if (= a (apply + ((fn factor [x] (filter #(zero? (rem x %)) (range 1 x))) a))) true false)))




(def alklo-157index #(map vector % (iterate inc 0)))

((fn [x y] (< (count x) (count y))) "pear" "plum")


(def alklo-kerensbets157 #(map vector % (iterate inc 0)))

;;(second (sort (clojure.set/intersection (set (factor %)) (set factor %2))))

;;(second (sort (clojure.set/intersection (set (factor 1023)) (set ( factor 858)) )))

(def banyakangka "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(def banyakangkapisah (map read-string (map str (seq banyakangka))))


; probeuler no 8 ((fn [xs] (loop [all xs angka [] result []] (if (<= 12 (count all)) result (recur (rest all) (take 13 all) (conj result angka)))))) (range 100))


(def euler2 ( sum (filter even? (take-while #(> 4000000 %) (alklo-fibo 100)))))

(defn euler12 [x] (map #(apply + %) ( map #(take % (iterate inc 1)) (range 1 x))))

(def eulergrid (/ (/ (faktorial 40) (faktorial 20)) (faktorial 20)))


;(filter (comp #(> % 500) (partial count) (partial factor)) (map #(apply + %) (euler10 10)))


; euler12 (map (juxt identity (comp (partial count) (partial factor))) ((drop 3000 (euler12 5000))))