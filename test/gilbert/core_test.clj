(ns gilbert.core-test
  (:require [clojure.test :refer :all]
            [gilbert.core :refer :all]))

(deftest arity-test
  (testing "fixed arities through 5"
    (is (= (map arity [(fn [] nil) (fn [_1] nil) (fn [_1 _2] nil)
                       (fn [_1 _2 _3] nil) (fn [_1 _2 _3 _4] nil) (fn [_1 _2 _3 _4 _5] nil)])
           (range 6)))))

;; test function for reactor - max
(defn cmax [a b] (if (> a b) [a a] [b b]))
(defn stop-max [reactor] (apply = (dosync @(:reactants reactor))))
(defn extract-max [reactor] (first @(:reactants reactor)))

(deftest get-reactions-test
  (let [rctr (reactor [cmax, cmax, cmax] [] (range 9) stop-max)
        _ (get-reactions rctr)
        rctns (dosync @(:reactions rctr))]
    (is (= 4 (count rctns)))))

(deftest clear-reactions-test
  (let [rctr (reactor [cmax, cmax, cmax] [] (range 9) stop-max)
        _ (get-reactions rctr)
        _ (clear-reactions rctr)
        rctns (dosync @(:reactions rctr))]
    (is (= 0 (count rctns)))))

(deftest process-reactions-test
  (let [rctr (reactor [cmax, cmax, cmax] [] (range 9) stop-max)
        _ (get-reactions rctr)
        _ (process-reactions rctr)
        rctnts (dosync @(:reactants rctr))]
    (is (= 9 (count rctnts)))))

(deftest cycle-reactor-test
  (let [rctr (reactor [cmax, cmax, cmax] [] (range 9) stop-max)
        _ (cycle-reactor rctr)]
    (is ((:stop-fn rctr) rctr))))

;; full tests
(deftest max-test
  (is (= 8 (gilbert [cmax, cmax, cmax] (range 9) stop-max extract-max))))


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
  (first @(:reactants reactor)))

(defn mergesort-stop [reactor]
  (let [data @(:reactants reactor)]
    (and (<= (count data) 1) (sorted (first data)))))

(deftest mergesort-test
  (is (= (range 100)
         (gilbert
           (concat (repeat 3 split-data) (repeat 3 merge-data))
           [(shuffle (range 100))]
           mergesort-stop
           mergesort-extract))))

;; ;; now test that the parallel version gets the same answers as the sequential version
;; ;; now make sure algorithms are more efficient, too...

;; (defn cmax2 [a b] (if (> a b) [a] [b]))

;; (deftest p-max-8
;;   (testing "max 0..n"
;;     (are [rctr rslt ] (= rctr rslt)
;;          (p-gilbert (vec (repeat 2 cmax2)) (vec (range 9)) stop-max extract-max) 8
;;          (p-gilbert (vec (repeat 7 cmax2)) (vec (range 99)) stop-max extract-max) 98
;;          )))

;; (deftest p-gilbert-test
;;   (testing "max 0..n"
;;     (are [rctr rslt ] (= rctr rslt)
;;          (gilbert (vec (repeat 2 cmax)) (vec (range 9)) stop-max extract-max) (p-gilbert (vec (repeat 2 cmax2)) (vec (range 9)) stop-max extract-max)
;;          (gilbert (vec (repeat 8 cmax)) (vec (range 99)) stop-max extract-max) (p-gilbert (vec (repeat 8 cmax2)) (vec (range 99)) stop-max extract-max)
;;          )))

;; ;; now, more efficient mergesort. Represent vectors as tuples { :sorted <bool> :data <vector> }
;; (defn sorted2 [v] (or (:sorted v) (= 1 (count (:data v)))))

;; (defn split-data2 [v]
;;   (if (sorted2 v)
;;     [v]
;;     (let [c (quot (count (:data v)) 2)]
;;       [{:sorted false :data (subvec (:data v) 0 c)} {:sorted false :data (subvec (:data v) c)}])))


;; (defn merge-data2 [a b]
;;   (if (and (sorted2 a) (sorted2 b))
;;       [{:sorted true :data (prim-merge < (:data a) (:data b))}]
;;         [a b]))

;; (defn mergesort-extract2 [reactor]
;;   (:data (first (:data reactor))))

;; (defn mergesort-stop2 [reactor]
;;   (let [data (:data reactor)]
;;     (and (<= (count data) 1) (sorted2 (first data)))))

;; (deftest p-mergesort-test
;;   (testing "permutations of mergesort"
;;     (are [x y] (= x (vec (range y)))
;;          (p-gilbert (concat (vec (repeat 3 split-data2))
;;                             (vec (repeat 3 merge-data2)))
;;                     [{:sorted false :data (vec (shuffle (range 100)))}]
;;                     mergesort-stop2
;;                     mergesort-extract2) 100)))


;; ;; Check proper building of reactor for l-gilbert

;; (defn stop-max-l [rctr]
;;   (apply = (elements rctr)))

;; (deftest make-reactor-test []
;;   (let [rctr (make-reactor (repeat 3 cmax2) (vec (range 9)) stop-max-l)]
;;     ;(println rctr (count (:functions rctr)) (:bound rctr) (:data rctr) (:stop-flag rctr))
;;     (are [x] (= x true)
;;          (= 6 (count @(:functions rctr)))
;;          (= 9 (count @(:data rctr)))
;;          (= 0 (count @(:bound rctr)))
;;          (= false @(:stop-flag rctr)))))

;; (deftest random-select-test []
;;   (is (= 2 (count (random-select 2 (ref (shuffle (range 9))))))))

;; (deftest random-bind-test []
;;   (let [rctr (make-reactor (repeat 3 cmax2) (vec (range 9)) stop-max-l)]
;;     (is (= 9 (count (elements rctr))))
;;     ;(println rctr)
;;     (is (= 2 (let [a1 (random-bind 2 (:bound rctr) (:data rctr))] (count a1))))
;;     ;(println rctr)
;;     (is (= 3 (let [a1 (random-bind 3 (:bound rctr) (:data rctr))] (count a1))))
;;     ;(println rctr)
;;     (is (= ned (random-bind 8 (:bound rctr) (:data rctr))))))

;; (deftest l-gilbert-test []
;;   (is (= 8 (l-gilbert (repeat 3 cmax2) (vec (range 9)) stop-max-l extract-max))))

;; (l-gilbert-test)
;; (run-all-tests)
