(defn genVector [value length] (vec (repeat length value)))

(defn vectorBinaryOp
  [operation, v1, v2]
  (mapv operation v1 v2 ))

(defn v+ [v1, v2] (vectorBinaryOp + v1 v2))
(defn v- [v1, v2] (vectorBinaryOp - v1 v2))
(defn v* [v1, v2] (vectorBinaryOp * v1 v2))

(defn v*s
  [v, s]
  (v* v (genVector s (count v))))

(defn scalar [v1, v2] (reduce + (v* v1 v2)))

(defn vect [vector1 vector2]
  (mapv (fn [i] (let [l (mod (+ i 1) 3)
                      r (mod (+ i 2) 3)]
                  (- (* (vector1 l) (vector2 r))
                     (* (vector1 r) (vector2 l)))))
        (range 0 3)))


(defn genMatrix [val, length, width]
  (vec (repeat length (genVector val width))))

(defn matrixBinaryOperation
  [operation, m1, m2]
  (mapv operation m1 m2))


(defn m+ [m1, m2] (matrixBinaryOperation v+ m1 m2))
(defn m- [m1, m2] (matrixBinaryOperation v- m1 m2))
(defn m* [m1, m2] (matrixBinaryOperation v* m1 m2))

(defn m*s
  [matrix,  scalar]
  (m* matrix (genMatrix scalar (count matrix) (count (first matrix)))))

(defn m*v [matrix vector] (mapv (partial scalar vector) matrix))

(defn transpose [matrix] (apply mapv vector matrix))

(defn m*m
  [m1, m2]
  (transpose (mapv #(m*v m1 %) (transpose m2))))

(defn shapelessBinaryOperation [op, s1, s2]
  (cond
    (number? s1) (op s1 s2)
    :else (mapv  (partial shapelessBinaryOperation op) s1 s2)))

(defn s+ [s1, s2] (shapelessBinaryOperation + s1 s2 ))
(defn s- [s1, s2] (shapelessBinaryOperation - s1 s2 ))
(defn s* [s1, s2] (shapelessBinaryOperation * s1 s2 ))
