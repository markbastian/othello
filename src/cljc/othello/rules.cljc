(ns othello.rules)

(defn boardwalk [board player-color start-cell dir]
  (let [f (disj #{:b :w} player-color) g #{player-color}]
    (when (= :e (get-in board start-cell))
      (loop [c (mapv + start-cell dir) res []]
        (let [x (get-in board c)]
          (cond
            (f x) (recur (mapv + c dir) (conj res c))
            (g x) res
            :default nil))))))

(defn analyze [board color]
  (into {}
        (let [directions [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
          (for [i (range (count board)) j (range (count (board i))) d directions
                :let [b (boardwalk board color [i j] d)]
                :when (not-empty b)]
            [[i j] (set b)]))))

(defn update-valid-moves [{:keys [board player] :as state}]
  (assoc state :valid-moves (analyze board player)))

(defn score [board] (dissoc (frequencies (flatten board)) :e))

(defn update-score [{:keys [board] :as state}]
  (assoc state :score (score board)))

(def other-player {:b :w :w :b})

(defn take-space [{:keys [valid-moves board player] :as state} ij]
  (if-some [flips (valid-moves ij)]
    (-> state
        (assoc :board (reduce #(assoc-in %1 %2 player) board (conj flips ij)))
        (update :player other-player)
        update-valid-moves
        update-score)
    state))

(defn winner [{:keys [valid-moves board]}]
  (when (empty? valid-moves)
    (let [{:keys [w b]} (score board)]
      (cond
        (> w b) :w
        (< w b) :b
        :default :tie))))

(def empty-board
  '[[:e :e :e :e :e :e :e :e]
    [:e :e :e :e :e :e :e :e]
    [:e :e :e :e :e :e :e :e]
    [:e :e :e :w :b :e :e :e]
    [:e :e :e :b :w :e :e :e]
    [:e :e :e :e :e :e :e :e]
    [:e :e :e :e :e :e :e :e]
    [:e :e :e :e :e :e :e :e]])

(def initial-state
  (update-score (update-valid-moves {:board empty-board :player :w})))

(defn expand [paths]
  (for [path paths
        :let [{:keys [valid-moves player] :as initial-state} (peek path)]
        [c] valid-moves]
    (into path [[player c] (take-space initial-state c)])))

(defn deep-search [state] (iterate expand [[state]]))