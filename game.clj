


(def GAME_RESULTS
  {
   -1 "Lose First Player"
   1  "Win Fist Player"
   -2 "Lose Second player"
   2  "Win Second player"
   0  "DRAW"
   })

(def CELLS
  {
   :EMPTY "E"
   :P1 "X"
   :P2 "O"
   })

(defn *getPlayerCell [no]
  (if (= no 1) (:P1 CELLS) (:P2 CELLS)))

(def MOVE_RESULTS
  {
   :WIN   1
   :LOSE -1
   :DRAW  0
   :MOVE_AGAIN 3
   :NEXT 4
   })

(definterface IMove
  (getRow [])
  (getColumn []))

(deftype Move [row column]
  IMove
  (getRow [this] row)
  (getColumn [this] column)
  )

(definterface ICell
  (getValue [])
  (setValue [newValue]))

(deftype Cell [^{:unsynchronized-mutable true} value]
  ICell
  (getValue [this] value)
  (setValue [this, newValue] (set! value newValue)))

(definterface IField
  (getCell [r, c])
  (putCell [r, c, cell])
  (toStr [])
  )

(deftype Field [field]
  IField
  (getCell [this, r, c] (nth (nth field r) c))
  (putCell [this, r, c, cell] (.setValue (nth (nth field r) c) (.getValue cell)))
  (toStr [this]
    (apply str (mapv (fn [row] (str (apply str (mapv (fn [x] (str (.getValue x) " ")) row)) "\n")) field))
    )
  )

(defn newField [n, m]
  (Field. (mapv (fn [r] (mapv (fn [c] (Cell. (:EMPTY CELLS))) (range m))) (range n))))

(definterface IBoardConfiguration
  (getRowSize [])
  (getColumnSize [])
  (isValid [move])
  (checkForWin [])
  (checkForDraw [])
  (makeMove [move, playerNo])
  (getPosition []))

(defn *nextTurn [turn] (if (= turn 1) 2 1))

(deftype Board [row, column, field,
                ^{:unsynchronized-mutable true} turnNo
                ^{:unsynchronized-mutable true} freeCells]
  IBoardConfiguration
  (getRowSize [this] row)
  (getColumnSize [this] column)
  (isValid [this, move]

    (and
      (= (type move) Move)
      (< (.getRow move) row)
      (>= (.getRow move) 0)
      (< (.getColumn move) column)
      (>= (.getColumn move) 0)
      (= (.getValue (.getCell field (.getRow move) (.getColumn move))) (:EMPTY CELLS))
      )
    )
  (checkForWin [this] false)
  (checkForDraw [this] (= freeCells 0))
  (makeMove [this, move, playerNo]
    (if
      (= (and (.isValid this move) (= turnNo playerNo)) false) (:LOSE MOVE_RESULTS)
                                                               (do
                                                                 (.putCell field (.getRow move) (.getColumn move) (Cell. (*getPlayerCell turnNo)))
                                                                 (set! turnNo (*nextTurn turnNo))
                                                                 (set! freeCells (- freeCells 1))
                                                                 (cond
                                                                   (.checkForWin this) (:WIN MOVE_RESULTS)
                                                                   (.checkForDraw this) (:DRAW MOVE_RESULTS)
                                                                   :else (:NEXT MOVE_RESULTS)
                                                                   )
                                                                 )
                                                               ))

  (getPosition [this] {
                       :rowSize row
                       :columnSize column
                       :field (.-field field)
                       :fieldStr (.toStr field)
                       :isValid (fn [move] (.isValid this move))
                       }
    )
  )
(defn newBoard [row, column] (Board. row column (newField row column) 1 (* row column)))

(definterface IPlayer
  (move [position]))

(definterface IGameServer
  (play [board]))


(deftype GameServer [player1, player2]
  IGameServer
  (play [this, board]
    (letfn
      [(moveWhile [currRes, player, no]
         (if
           (= currRes (:MOVE_AGAIN MOVE_RESULTS))
           (moveWhile (.makeMove board (.move player (.getPosition board)) no) player no)
           currRes))
       (movePlayer [player, no] (moveWhile (:MOVE_AGAIN MOVE_RESULTS) player no))
       (playUntilEnd [player1 player2]
         (let [r1 (movePlayer player1, 1)]
           (if (not= r1 (:NEXT MOVE_RESULTS))
             r1
             (let [r2 (movePlayer player2 2)]
               (if (not= r2 (:NEXT MOVE_RESULTS))
                 r2
                 (playUntilEnd player1 player2))))))
       ]
      (playUntilEnd player1 player2)
      )
    )
  )

(deftype AIPlayer []
  IPlayer
  (move [this, position]
    (loop [i 0 j 0]
      (cond
        ((:isValid position) (Move. i j))  (Move. i j)
        (= j (:columnSize position)) (recur (+ i 1) 0)
        :else (recur i (+ j 1))
        )
      )
    ))
