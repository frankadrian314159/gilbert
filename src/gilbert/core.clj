(ns gilbert.core)

(defn arity
 "Returns the maximum parameter count of each invoke method found by refletion
  on the input instance. The returned value can be then interpreted as the arity
  of the input function. The count does NOT detect variadic functions."
  [f]
  (let [invokes (filter #(= "invoke" (.getName %1)) (.getDeclaredMethods (class f)))]
  (apply max (map #(alength (.getParameterTypes %1)) invokes))))

(defn load-reactor [functions data stop-fn]
  ;(println "Loading reactor - data: " data)
  {:functions (shuffle functions) :data (shuffle data) :stop-fn stop-fn})

(defn reaction-cycle [reactor]
  (let [f (first (:functions reactor))
        ;_ (println f)
        a (arity f)
        args (take a (:data reactor))
        rest (drop a (:data reactor))]
    ;(println f a args rest)
    (load-reactor (:functions reactor) (concat (apply f args) rest) (:stop-fn reactor))))

(defn get-sub-reactors [reactor]
  (loop [subs []
         rems reactor]
    (if (= 0 (count (:functions rems)))
      [rems subs]
      (let [f (first (:functions rems))
            a (arity f)]
        (if (< (count (:functions rems)) a)
          [rems subs]
          (let [args (take a (:data rems))]
            (recur (conj subs (concat [f] args))
                   {:functions (rest (:functions rems)) :data (drop a (:data rems))})))))))

(defn p-reaction-cycle [reactor]
  (let [[remainders subs] (get-sub-reactors reactor)
        mapped (pmap #(apply (first %) (rest %)) subs)
        leftover-data (:data remainders)
        new-data (reduce concat leftover-data mapped)]
    ;(println "In p-reaction-cycle: " subs leftover-data)
    (load-reactor (:functions reactor) new-data (:stop-fn reactor))))

(defn run-reactor [reactor]
  (loop [r reactor]
    ;(println "Cycling reactor - data: " (:data r))
    (if (apply (:stop-fn r) [r])
      r
      (recur (p-reaction-cycle r)))))


(defn gilbert [functions data stop-fn extract-fn]
  "The gilbert chemical programing system"
  (let [reactor (load-reactor functions data stop-fn)]
    (extract-fn (run-reactor reactor))))


;; test functions - max
(defn cmax [a b] (if (> a b) [a a] [b b]))
(defn stop-max [reactor] (apply = (:data reactor)))
(defn extract-max [reactor] (first (:data reactor)))

;(time (gilbert (vec (repeat 2 cmax)) (vec (range 9)) stop-max extract-max))
;(time (gilbert (vec (repeat 8 cmax)) (vec (range 99)) stop-max extract-max))
;(time (gilbert (vec (repeat 100 cmax)) (vec (range 10000)) stop-max extract-max))
;(time (gilbert (vec (repeat 300 cmax)) (vec (range 100000)) stop-max extract-max))

;; test functions - merge sort
(defn prim-merge [pred left right]
    (loop [v [] l left r right]
        (if (and (seq l) (seq r))
            (if (pred (first l) (first r))
                (recur (conj v (first l)) (rest l) r)
                (recur (conj v (first r)) l (rest r)))
            (vec (concat v l r)))))

(defn sorted [a]
  (or (<= (count a) 1) (apply < a)))

(defn split-data [v]
  (if (sorted v)
    [v]
    (let [c (quot (count v) 2)]
      [(subvec v 0 c) (subvec v c)])))


(defn merge-data [a b]
  (if (and (sorted a) (sorted b))
      [(prim-merge < a b)]
        [a b]))

(defn mergesort-extract [reactor]
  (first (:data reactor)))

(defn mergesort-stop [reactor]
  (let [data (:data reactor)]
    (and (<= (count data) 1) (sorted (first data)))))

;(time (gilbert (concat (vec (repeat 3 split-data)) (vec (repeat 3 merge-data))) [(vec (shuffle (range 100)))] mergesort-stop mergesort-extract))
;(time (gilbert (concat (vec (repeat 5 split-data)) (vec (repeat 5 merge-data))) [(vec (shuffle (range 1000)))] mergesort-stop mergesort-extract))
;(time (gilbert (concat (vec (repeat 50 split-data)) (vec (repeat 50 merge-data))) [(vec (shuffle (range 100000)))] mergesort-stop mergesort-extract))
