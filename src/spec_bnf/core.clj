(ns spec-bnf.core
  (:require [clojure.spec :as s]
            [clojure.string :as str]))

(s/def ::type keyword?)

(s/def ::id (s/or :kw keyword? :sym symbol?))

(s/def ::identifier
  (s/and (s/keys :req [::type ::id])
         #(= :identifier (::type %))))

(s/def ::term (s/or :kw keyword? :sym symbol?))

(s/def ::terminal
  (s/and (s/keys :req [::type ::id])
         #(= :identifier (::type %))))

(s/def ::optional
  (s/and (s/keys :req [::type ::rhs])
         #(= :optional (::type %))))

(s/def ::repetition
  (s/and (s/keys :req [::type ::rhs])
         #(= :repetition (::type %))))

(s/def ::rhss
  (s/every ::rhs))

(s/def ::alternation
  (s/and (s/keys :req [::type ::rhss])
         #(= :alternation (::type %))))

(s/def ::concatenation
  (s/and (s/keys :req [::type ::rhss])
         #(= :concatenation (::type %))))

(s/def ::lhs keyword?)

(s/def ::rhs
  (s/or ::identifier
        ::terminal
        ::optional
        ::repetition
        ::alternation
        ::concatenation))

(s/def ::rule
  (s/and (s/keys :req [::type ::lhs ::rhs])
         #(= :rule (::type %))))

(s/def ::rules
  (s/every ::rule))

(s/def ::grammar
  (s/and (s/keys :req [::type ::rules])
         #(= :grammar (::type %))))

(defn- surround-with-vector-terminals
  "Sourrounds a vector of parts with [ and ] terminals."
  [parts level]
  (if (zero? level)
    (into
      [{::type :terminal ::term "["}]
      (conj parts {::type :terminal ::term "]"}))
    parts))

(s/def ::spec (s/or :kw keyword?
                    :sym symbol?
                    :set set?
                    :list sequential?))

(s/fdef rhs
  :args (s/cat :spec ::spec :level int?)
  :ret ::rhs)

(defn rhs
  "Takes a spec and returns an EBNF rhs."
  [spec level]
  (let [[type spec] (s/conform ::spec spec)]
    (case type
      (:kw :sym)
      {::type :identifier
       ::id spec}

      :set
      (case (count spec)
        0
        (throw (Exception. "Empty set"))

        1
        {::type :terminal
         ::term (first spec)}

        {::type :alternation
         ::rhss (mapv #(hash-map ::type :terminal ::term %) spec)})

      :list
      (let [[fn-sym & args] spec]
        (case fn-sym
          clojure.spec/cat
          {::type :concatenation
           ::rhss
           (-> (into
                 []
                 (comp (map second)
                       (map #(rhs % (inc level))))
                 (partition 2 args))
               (surround-with-vector-terminals level))}

          clojure.spec/and
          (rhs (first args) level)

          clojure.spec/or
          {::type :alternation
           ::rhss
           (into
             []
             (comp (map second)
                   (map #(rhs % 0)))
             (partition 2 args))}

          clojure.spec/?
          {::type :optional
           ::rhs (rhs (first args) level)}

          clojure.spec/*
          {::type :repetition
           ::rhs (rhs (first args) level)}

          clojure.spec/+
          {::type :concatenation
           ::rhss
           (let [ebnf (rhs (first args) level)]
             [ebnf
              {::type :repetition
               ::rhs ebnf}])}

          clojure.spec/spec
          (rhs (first args) level)

          clojure.spec/multi-spec
          (let [mm (eval (first args))
                methods (.getMethodTable mm)
                dispatch-fn (.dispatchFn mm)
                gen-val #(condp = dispatch-fn first [%])]
            {::type :alternation
             ::rhss (map #(rhs (s/form (mm (gen-val %))) level) (keys methods))}))))))

(s/fdef rule
  :args (s/cat :spec-kw keyword?)
  :ret ::rule)

(defn rule
  "Creates an EBNF rule from the spec referenced by the spec keyword.

  The spec keyword is used as left-hand-side of the rule."
  [spec-kw]
  {::type :rule
   ::lhs spec-kw
   ::rhs (rhs (s/form spec-kw) 0)})

(s/fdef grammar
  :args (s/cat :spec-kws (s/* keyword?)))

(defn grammar
  "Takes spec keywords and returns an EBNF grammar with one rule for each spec.

  See: rule"
  [& spec-kws]
  {::type :grammar
   ::rules (map rule spec-kws)})

(defn emit-identifier [identifier ns-aliases]
  (let [ns (namespace identifier)
        alias (get ns-aliases ns ns)]
    (if alias
      (str alias "/" (name identifier))
      (name identifier))))

(declare emit-str)

(defn emit-rule [rule indent opts]
  (let [lhs (emit-identifier (::lhs rule) (:ns-aliases opts))]
    (str lhs (apply str (repeat (- indent (count lhs)) " "))
         " = "
         (emit-str (::rhs rule) 0 opts))))

(defn emit-str
  [ebnf level opts]
  (case (::type ebnf)
    :concatenation
    (let [s (str/join " , " (map #(emit-str % (inc level) opts) (::rhss ebnf)))]
      (if (pos? level)
        (str "(" s ")")
        s))

    :alternation
    (let [s (str/join " | " (map #(emit-str % (inc level) opts) (::rhss ebnf)))]
      (if (pos? level)
        (str "(" s ")")
        s))

    :optional
    (str "[ " (emit-str (::rhs ebnf) 0 opts) " ]")

    :repetition
    (str "{  " (emit-str (::rhs ebnf) 0 opts) " }")

    :identifier
    (let [identifier (::id ebnf)]
      (if (or (keyword? identifier) (symbol? identifier))
        (emit-identifier identifier (:ns-aliases opts))
        (str identifier)))

    :terminal
    (let [terminal (::term ebnf)]
      (if (sequential? terminal)
        (if (= 'quote (first terminal))
          (str "\"" (name (second terminal)) "\"")
          (throw (Exception. (str "Unsupported terminal: " (pr-str terminal)))))
        (str "\"" terminal "\"")))

    (throw (Exception. (str "Unsupported EBNF: " (pr-str ebnf))))))

(defn spec-kws
  "Returns all registered spec keywords of namespace."
  [ns]
  (into
    []
    (comp (filter #(= ns (namespace %))) (filter keyword?))
    (keys (s/registry))))

(defn emit-grammar
  {:arglists '([grammar & {:as opts}])}
  [{:keys [::rules]} & {:keys [ns-aliases] :as opts}]
  (let [indent (apply max (map count (map #(emit-identifier % ns-aliases) (map ::lhs rules))))]
    (str/join "\n" (map #(emit-rule % indent opts) rules))))
