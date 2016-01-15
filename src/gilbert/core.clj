(ns gilbert.core)

(defn arity
 "Returns the maximum parameter count of each invoke method found by refletion
  on the input instance. The returned value can be then interpreted as the arity
  of the input function. The count does NOT detect variadic functions."
  [f]
  (let [invokes (filter #(= "invoke" (.getName %1)) (.getDeclaredMethods (class f)))]
  (apply max (map #(alength (.getParameterTypes %1)) invokes))))

(defn reactor [formulae reactions reactants stop-fn]
  (let [r {:formulae formulae
           :arity-tally (apply + (map arity formulae))
           :reactions (ref reactions)
           :reactants (ref reactants)
           :stop-fn stop-fn}]
    r))

(defn set-stop-flag [bool rctr]
  (dosync (alter (:stop-flag rctr) (fn [_] bool))))

(defn clear-reactions [rctr]
  (dosync (ref-set (:reactions rctr) ())))

(defn obtain-reactants [rctr]
  (dosync
    (let [r @(:reactants rctr)]
      (alter (:reactants rctr) (fn [_] ()))
      r)))

(defn get-reactions [rctr]
  (let [arg-arity (map arity (:formulae rctr))
        arg-tally (reduce + arg-arity)
        shuffled-reactants (shuffle (obtain-reactants rctr))
        parts (partition arg-tally arg-tally [] shuffled-reactants)]
    (letfn [(process-partition [part]
              (if (= (count part) arg-tally)
                (loop [reactions []
                       remaining part
                       f (:formulae rctr)
                       ar arg-arity]
                  (if (empty? f)
                    (dosync (alter (:reactions rctr) concat reactions))
                    (let [arity (first ar)
                          args (take arity remaining)]
                      (recur (conj reactions{:function (first f) :args args})
                             (drop arity remaining)
                             (rest f)
                             (rest ar)))))
                (loop [reactions []
                       remaining part]
                  (let [f (rand-nth (:formulae rctr))
                        a (arity f)
                        args (take a remaining)]
                    (if (= (count args) a)
                      (recur (conj reactions {:function f :args args}) (drop a remaining))
                      (dosync
                        (alter (:reactions rctr) concat reactions)
                        (alter (:reactants rctr) (fn [_] remaining))))))))]
     (doall (map process-partition parts)))))

(defn process-reactions [rctr]
  (letfn [(react [reaction]
            (let [result (apply (:function reaction) (:args reaction))]
              (dosync (alter (:reactants rctr) concat result))))]
    (doall (map react (dosync @(:reactions rctr))))))

(defn step-reactor [rctr]
  (println rctr)
  (get-reactions rctr)
  (println rctr)
  (process-reactions rctr)
  (println rctr)
  (clear-reactions rctr))

(defn cycle-reactor [rctr]
  (loop []
    (if ((:stop-fn rctr) rctr)
      rctr
      (do
        (step-reactor rctr)
        (recur)))))

(defn gilbert [formulae reactants stop-fn extract-fn]
  "The gilbert chemical programing system"
  (let [rctr (reactor formulae [] reactants stop-fn)
        _ (cycle-reactor rctr)]
    (extract-fn rctr)))

