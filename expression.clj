
(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args]
    (apply proto-call this key args)))


(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))
(def _func (field :func))
(def _operands (field :operands))

(defn Constant [val]
  {
   :val      val
   :evaluate (fn [this, args] (this :val))
   :diff     (fn [this, x] (Constant 0))
   :toString (fn [this] (format "%.1f" (double (this :val))))
   })

(defn Variable [name]
  {
   :name     name
   :evaluate (fn [this, args] (get args (this :name)))
   :diff     (fn [this, x] (if (= x (this :name)) (Constant 1) (Constant 0)))
   :toString (fn [this] (this :name))
   })

(defn pushToOperands
  ([operands, function, args] (mapv (fn [operand] (function operand args)) operands))
  ([operands, function] (mapv (fn [operand] (function operand)) operands)))

(def _operationName (field :operationName))
(def _diffFunc (field :diffFunc))
(def operationsPrototype
  {
   :evaluate (fn [this, args]
               (apply (_func this) (pushToOperands (_operands this) evaluate args)))
   :diff     (fn [this, x]
               (if (= 1 (count (_operands this)))
                 (apply ((_diffFunc this) x) (_operands this))
                 (reduce ((_diffFunc this) x) (_operands this))))
   :toString (fn [this] (str "(" (_operationName this) (apply str (pushToOperands (_operands this) (fn [x] (str " " (toString x))))) ")"))
   })


(defn abstractOperation [func, operationName, diffFunc]
  (fn [& operands]
    {
     :prototype     operationsPrototype
     :func          func
     :operationName operationName
     :operands      operands
     :diffFunc      diffFunc
     })
  )

(def Add (abstractOperation + "+" #(fn [u v] (Add (diff u %) (diff v %)))))
(def Subtract (abstractOperation - "-"  #(fn [u v] (Subtract (diff u %) (diff v %)))))
(def Multiply (abstractOperation * "*" #(fn [u v] (Add (Multiply (diff u %) v) (Multiply (diff v %) u)))))
(def Negate (abstractOperation (fn [x] (- x)) "negate" #(fn [u] (Negate (diff u %)))))
(defn zero-division [l r] (/ (double l) (double r)))

(def Divide (abstractOperation zero-division "/" #(fn [u v] (Divide (Subtract (Multiply (diff u %) v) (Multiply (diff v %) u)) (Multiply v v)))))
(def Exp (abstractOperation (fn [x] (Math/exp x)) "exp" #(fn [u] (Multiply (Exp u) (diff u %)))))
(defn exp [x] (Math/exp x))


(defn sumexp [& values] (apply + (mapv exp values)))
(def SumexpProto (abstractOperation sumexp "sumexp" #(fn [u v] (Add (diff (Exp u) %) (diff (Exp v) %)))))
(defn Sumexp [& operands]
  {
   :prototype operationsPrototype
   :operationName "sumexp"
   :operands operands
   :evaluate (fn [this, args] (evaluate (apply Add (mapv Exp (_operands this)) ) args))
   :diff (fn [this, x] (apply Add (mapv (fn [o] (diff (Exp o) x)) (_operands this)) ))
   })

(defn Softmax [& operands]
  {
    :operands operands
    :evaluate (fn [this, args] (evaluate  (Divide (Exp (first (_operands this))) (apply Sumexp (_operands this))) args))
    :toString (fn [this] (str "(softmax" (apply str (pushToOperands (_operands this) (fn [x] (str " " (toString x))))) ")"))
    :diff (fn [this, x] (diff (Divide (Exp (first (_operands this))) (apply Sumexp (_operands this))) x))
   })

(def OPERATIONS {'+ Add
                 '- Subtract
                 '* Multiply
                 '/ Divide
                 'negate Negate
                 'sumexp Sumexp
                 'softmax Softmax})

(defn parse [s] (cond
                  (seq? s ) (apply (OPERATIONS (first s)) (mapv parse (rest s)))
                  (number? s) (Constant s)
                  :else (Variable (str s))))

(defn parseObject[s]
  (parse (read-string s)))
  
  
  -------------------------------------------------------------------
  (ns cljhomeworks.core)
(defn constant [x] (fn [args] x))
(defn variable [x] (fn [args] (args x)))

(defn operation [f]
  (fn [& operands]
    (fn [args]
      (apply + (mapv #(% args) operands)))))


(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(def divide (operation /))
(def OPERATORS
  {'+ add
   '- subtract
   '/ divide
   '* multiply
   })
(defn parse[s]
  (cond
    (seq? s) (apply  (OPERATORS (first s)) (mapv parse (rest s)))
    (number? s) (constant s)
    :else (variable (str s))
    )
  )

(defn parseFunction[s]
  (parse (read-string s)))

