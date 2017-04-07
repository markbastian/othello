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
        (update :ply inc)
        (cond-> (#{:b} player) (update :turn inc))
        update-valid-moves
        update-score)
    state))

(defn game-over? [{:keys [valid-moves]}] (empty? valid-moves))

(defn winner [{:keys [board] :as state}]
  (when (game-over? state)
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
  (update-score (update-valid-moves {:board empty-board :player :w :ply 0 :turn 0})))

(defn expand [paths]
  (for [path paths
        :let [{:keys [valid-moves player] :as initial-state} (peek path)]
        [c] valid-moves]
    (into path [[player c] (take-space initial-state c)])))

(defn deep-search [state] (iterate expand [[state]]))

(defn turn-summary [turn-vec]
  (mapv (fn [{:keys [score] :as m}] (or score m)) turn-vec))

(defn lookahead [state n]
  (mapv turn-summary (nth (deep-search state) n)))

(defn utility [score player]
  (- (player score) (-> player other-player score)))

(defn minimax
  ([{:keys [valid-moves score] :as state} player maximizing? decision-vec max-depth]
   (if (or (game-over? state) (> (count decision-vec) max-depth))
     (conj decision-vec (utility score player))
     (let [obj-fn (if maximizing? max-key min-key)
           moves (for [[c] valid-moves]
                   (minimax (take-space state c)
                            player (not maximizing?)
                            (conj decision-vec c) max-depth))]
       (apply obj-fn peek moves))))
  ([{:keys [player] :as initial-state} max-depth]
   (minimax initial-state player true [] max-depth)))

(def pinf #?(:clj Double/POSITIVE_INFINITY :cljs js/Number.POSITIVE_INFINITY))
(def ninf #?(:clj Double/NEGATIVE_INFINITY :cljs js/Number.NEGATIVE_INFINITY))

(defn alphabeta
  ([{:keys [valid-moves score] :as state} player alpha beta maximizing? decision-vec max-depth]
   (cond
     (or (game-over? state) (> (count decision-vec) max-depth)) (conj decision-vec (utility score player))
     maximizing? (reduce (fn [v [c]]
                           (let [x (alphabeta (take-space state c) player (max alpha (peek v)) beta (not maximizing?) (conj decision-vec c) max-depth)
                                 vn (max-key peek v x)]
                             (if (<= beta (max alpha (peek vn))) (reduced vn) vn)))
                         [ninf] valid-moves)
     :default (reduce (fn [v [c]]
                        (let [x (alphabeta (take-space state c) player alpha (min beta (peek v)) (not maximizing?) (conj decision-vec c) max-depth)
                              vn (min-key peek v x)]
                          (if (<= (min beta (peek vn)) alpha) (reduced vn) vn)))
                      [[pinf] beta] valid-moves)))
  ([{:keys [player] :as initial-state} max-depth]
   (alphabeta initial-state player ninf pinf true [] max-depth)))


#_(defn alphabeta
  ([{:keys [valid-moves score] :as state} player alpha beta maximizing? decision-vec max-depth]
   (cond
     (or (game-over? state) (> (count decision-vec) max-depth)) (conj decision-vec (utility score player))
     maximizing? (first
                   (reduce (fn [[v a] [c]]
                             (let [x (alphabeta (take-space state c) player a beta (not maximizing?) (conj decision-vec c) max-depth)
                                   vn (max-key peek v x)
                                   an (max a (peek vn))]
                               (if (<= beta an) (reduced [vn an]) [vn an])))
                           [[ninf] alpha] valid-moves))
     :default (first
                (reduce (fn [[v b] [c]]
                          (let [x (alphabeta (take-space state c) player alpha b (not maximizing?) (conj decision-vec c) max-depth)
                                vn (min-key peek v x)
                                bn (min beta (peek vn))]
                            (if (<= bn alpha) (reduced [vn bn]) [vn bn])))
                        [[pinf] beta] valid-moves))))
  ([{:keys [player] :as initial-state} max-depth]
   (alphabeta initial-state player ninf pinf true [] max-depth)))
