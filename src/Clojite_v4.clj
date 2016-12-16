(ns Clojite_v4
  (:require [game]
            [io])
  (:gen-class))


;; Changes
;; ===
;; 1. Tried to speed up the bot
;;    - have a filtered-not-me-map 
;;    - remove an extra map that was not doing anything
;; 2. Added a value function for chosing the "weak-site".
;;    - It chooses the best site based on production
;;    - It helps in the early game (higher production)
;;    - It helps in the late game (get back the higher production sites first)

(def bot-name "Clojite v4")
(def filtered-not-me-map (atom {}))

(defn get-at [game-map x y]
  (let [game-size (count game-map)
        real-x (mod x game-size)
        real-y (mod y game-size)] 
    (try
     (-> game-map
         (nth real-y)
         (nth real-x))
     (catch Exception e (-> game-map (nth 0) (nth 0))))))

(defn get-north-site [game-map site]
  (get-at game-map (:x site) (dec (:y site))))

(defn get-south-site [game-map site]
  (get-at game-map (:x site) (inc (:y site))))

(defn get-east-site [game-map site]
  (get-at game-map (inc (:x site)) (:y site)))

(defn get-west-site [game-map site]
  (get-at game-map (dec (:x site)) (:y site)))


(defn distance-generic 
  "A generic function for distance calculation"
  [map-size f a b]
  (let [dist1 (mod (- a b) map-size)
        dist2 (mod  (+ (- map-size a) b) map-size)]
      (f dist1 dist2)))

(defn distance-x
  "Calculate the distance between two sites along the x axis"
  [map-size a b]
  (distance-generic map-size min (:x a) (:x b)))

(defn direction-x 
  "Calculate the direction a strength must travel between two sites (a to b) along the x axis"
  [map-size a b]
  (let [dist-left (mod (- (:x a) (:x b)) map-size)
        dist-right (mod  (+ (- map-size (:x a)) (:x b)) map-size)]
    (if (< dist-left dist-right)
        :west
        :east)))

(defn distance-y
  "Calculate the distance between two sites along the y axis"
  [map-size a b]
  (distance-generic map-size min (:y a) (:y b)))

(defn direction-y 
  "Calculate the direction a strength must travel between two sites (a to b) along the x axis"
  [map-size a b]
  (let [dist-north (mod (- (:y a) (:y b)) map-size)
        dist-south (mod  (+ (- map-size (:y a)) (:y b)) map-size)]
    (if (< dist-north dist-south)
        :north
        :south)))

(defn distance 
  "Calculate the distance between two sites"
  [map-size a b]
  (let [dist-x (distance-x map-size a b)
        dist-y (distance-y map-size a b)]
   (+ dist-x dist-y)))

(defn get-closest-site 
  "Get the site closest to the current site. Expensive"
  [my-id game-map site]
  (let [map-size (count game-map)]
    (->> @filtered-not-me-map
         (reduce (fn [[r v] x]
                  (let [new-dist (distance map-size site x)] 
                    (if (< v new-dist) [r v] [x new-dist]))) 
                [{} 1000])
         (first))))

(defn get-closest-site-direction 
  "Get the direction of the closet site"
  [my-id game-map site]
  (let [closest-site (get-closest-site my-id game-map site)
        map-size (count game-map)
        dist-x (distance-x map-size site closest-site)
        dist-y (distance-y map-size site closest-site)
        dir-x (direction-x map-size site closest-site)
        dir-y (direction-y map-size site closest-site)]
    (cond
      (zero? dist-y) [site dir-x]
      (zero? dist-x) [site dir-y]
      :else [site (rand-nth [dir-x dir-y])])))


(defn get-lowest-strength-site
  "Given a list of sites finds the one with the lowest strength"
  [my-id sites]
  (let [filtered (->> sites
                      (filter #(not= (:owner %) my-id)))]
      (if (empty? filtered)
        nil
        (reduce #(if (< (:strength %1) (:strength %2)) %1 %2) filtered))))

(defn get-highest-production-site
  "Given a list of sites finds the one with the highest production"
  [my-id sites]
  (let [filtered (->> sites
                      (filter #(not= (:owner %) my-id)))]
      (if (empty? filtered)
        nil
        (reduce #(if (< (:production %1) (:production %2)) %2 %1) filtered))))

(defn site-value [site]
  (let [str-value (- 255 (:strength site))
        prod-value (* 2 (:production site))]
    (+ str-value prod-value)))

(defn get-highest-value-site
  "Given a list of sites finds the one with the highest production"
  [my-id sites]
  (let [filtered (->> sites
                      (filter #(not= (:owner %) my-id)))]
      (if (empty? filtered)
        nil
        (reduce #(if (< (site-value %1) (site-value %2)) %2 %1) filtered))))


(defn get-lowest-direction
  "Given a cell that I own figure out the closest cell next to me that is not owned by me. Send that way."
  [my-id game-map site]
  (if (and (< (:strength site) (* 6 (:production site))) (<= (:strength site) 250))
    [site :still]
    ;;TODO: Adjecent sites fn
    (let [north-site (get-north-site game-map site)
          east-site (get-east-site game-map site)
          south-site (get-south-site game-map site)
          west-site (get-west-site game-map site)
          weak-site (get-highest-value-site my-id [north-site east-site south-site west-site])]
      (if (nil? weak-site)
        (get-closest-site-direction my-id game-map site)
        (if (<= (:strength site) (:strength weak-site))
         [site :still]
         (cond 
           (= weak-site north-site)  [site :north]
           (= weak-site south-site)  [site :south]
           (= weak-site east-site)  [site :east]
           (= weak-site west-site)  [site :west]
           :else [site :north]))))))

(defn certain-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (let [my-sites (->> game-map
                      flatten
                      (filter #(= (:owner %) my-id)))]
    (->> my-sites
      (map #(get-lowest-direction my-id game-map %)))))

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (do
          (reset! filtered-not-me-map (->> game-map flatten (filter #(not= (:owner %) my-id))))   
          (io/send-moves! (certain-moves my-id game-map)))))))
