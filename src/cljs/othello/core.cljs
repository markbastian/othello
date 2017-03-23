(ns othello.core
  (:require [reagent.core :as reagent :refer [atom]]
            [othello.rules :as rules]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

(defonce state (atom rules/initial-state))

(defn render [state]
  (let [{:keys [board player valid-moves]} @state
        cell-dim 40]
    [:div
     [:svg {:width (* cell-dim (count board)) :height (* cell-dim (count board))}
      (doall (for [i (range (count board)) j (range (count (board i))) :let [c [i j]]]
               [:rect { :key (str i ":" j) :x (* i cell-dim) :y (* j cell-dim)
                       :width cell-dim :height cell-dim :stroke :red
                       :fill (if (valid-moves c) "#C0FFC0" :gray)
                       :on-click (fn [e] (swap! state rules/take-space c))}]))
      (doall (for [i (range (count board)) j (range (count (board i)))
                   :let [color (case (get-in board [i j]) :w :white :b :black nil)] :when color]
               [:circle { :key (str "p:" i ":" j ":" color) :cx (* (+ i 0.5) cell-dim) :cy (* (+ j 0.5) cell-dim)
                         :r (* 0.5 cell-dim) :fill color}]))]
     (if-some [winner (rules/winner @state)]
       [:h3 (str "Game over! " ({:w "White won!" :b "Black won!" :tie "It's a tie!"} winner))]
       [:h3 (str "It's " ({:w "white" :b "black"} player) "'s turn")])]))

(when-let [app-context (. js/document (getElementById "app"))]
  (let []
  (reagent/render-component [render state] app-context)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
