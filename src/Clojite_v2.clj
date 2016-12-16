(ns Clojite_v2
  (:require [game]
            [io])
  (:gen-class))

(def bot-name "Clojite v2")

(defn get-at [game-map x y]
  (let [game-size (count game-map)
        real-x (mod x game-size)
        real-y (mod y game-size)]
    (-> game-map
        (nth real-y)
        (nth real-x))))

(defn get-north-site [game-map site]
  (get-at game-map (:x site) (dec (:y site))))

(defn get-south-site [game-map site]
  (get-at game-map (:x site) (inc (:y site))))

(defn get-east-site [game-map site]
  (get-at game-map (inc (:x site)) (:y site)))

(defn get-west-site [game-map site]
  (get-at game-map (dec (:x site)) (:y site)))


(defn get-lowest-strength-site [my-id sites]
  "Given a list of sites finds the one with the lowest strength"
  (let [filtered (->> sites
                      (filter #(not= (:owner %) my-id)))]
      (if (empty? filtered)
        nil
        (reduce #(if (< (:strength %1) (:strength %2)) %1 %2) filtered))))

(defn get-lowest-direction
  "Given a cell that I own figure out the closest cell next to me that is not owned by me. Send that way."
  [my-id game-map [site orig-direction]]
  (if (< (:strength site) (* 4 (:production site)))
    [site :still]
    ;;TODO: Adjecent sites fn
    (let [north-site (get-north-site game-map site)
          east-site (get-east-site game-map site)
          south-site (get-south-site game-map site)
          west-site (get-west-site game-map site)
          weak-site (get-lowest-strength-site my-id [north-site east-site south-site west-site])]
      (if (nil? weak-site)
        [site (rand-nth [:west :north :still])]
        (cond 
          (= weak-site north-site)  [site :north]
          (= weak-site south-site)  [site :south]
          (= weak-site east-site)  [site :east]
          (= weak-site west-site)  [site :west])))))

(defn certain-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (let [my-sites (->> game-map
                      flatten
                      (filter #(= (:owner %) my-id)))]
    (->>
      (map vector my-sites (repeatedly #(rand-nth [:north])))
      (map #(get-lowest-direction my-id game-map %)))))

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (io/send-moves! (certain-moves my-id game-map))))))
