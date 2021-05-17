


(defn bConstant [x] (fn [args] x))
(defn bVariable [x] (fn [args] (args x)))

(defn bFunc [f]
  (fn [& operands]
    (fn [args]
      ( apply f (mapv #(% args) operands))
      )))

(def bAnd (bFunc bit-and))
(def bOr (bFunc bit-or))
(def bXor (bFunc bit-xor))
(def bNot (bFunc #(if (= 0 %) 1 0)))

(def mainExpr {})
(def Variables [])
(def VariablesMap {})

(def finalAnswer "")

(defn assocArgs [bArgs]
  (do
    (mapv #(def VariablesMap (assoc VariablesMap %1 %2)) Variables bArgs)
    (identity VariablesMap)))

(defn addToFinalAnswer [x] (def finalAnswer (str finalAnswer x)))

(defn correctDisjunct [bValue variableName]
  (if (= 0 bValue) (str variableName) (str "!" variableName)))

(defn addOperands [bArgs]
  (do
    (if (not= finalAnswer "") (addToFinalAnswer " & "))
    (addToFinalAnswer (str "(" (correctDisjunct (first bArgs) (first Variables))))
    (mapv #(addToFinalAnswer (str " | " (correctDisjunct %1 %2))) (rest bArgs) (rest Variables))
    (addToFinalAnswer ")")
    ))

(defn *test [bCombination]
  (if (= (mainExpr (assocArgs bCombination)) 0) (addOperands bCombination)))

(defn genAnswer [curr, varCount]
  (if
    (= (count curr) varCount)
    (*test curr)
    (do
      (genAnswer (conj curr 0) varCount)
      (genAnswer (conj curr 1) varCount))))

(defn resetAll []
  (do
    (def finalAnswer "")
    (def Variables [])
    (def VariablesMap {})
    (def mainExpr {})))

(defn solve []
  (do
    (def finalAnswer "")
    (genAnswer [] (count Variables))
    (identity finalAnswer)))

(def mainExpr (bOr (bAnd (bVariable "a") (bVariable "b")) (bNot(bVariable "c"))))
(def Variables ["a" "b" "c"])
(def VariablesMap {"a" 0 "b" 0 "c" 0 })
(println (solve))
