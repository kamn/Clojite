(ns Clojite_v5
  (:require [game]
            [io])
  (:gen-class))


;; Changes
;; ===
;; 1. Speed improvments
;;    - Added indexed-map for faster lookup
;; 2. Force squares to move if them + another is 255 or greater
;;    - This is an attempt to lose less points in merges
;; 3. Changed value to weight heavier higher production
;;    - 
;; 4. Say that enemies are closet so that middle sites send there strength in that direction

(def bot-name "Clojite v5")
(def max-str 255)
(def filtered-not-me-map (atom {}))
(def indexed-map (atom {}))


(defn get-at 
  "Get site info at a specific x and y"
  [game-map x y]
  (let [game-size (count game-map)
        real-x (mod x game-size)
        real-y (mod y game-size)
        site (get @indexed-map [real-x real-y])]
    (if (nil? site)
        (get @indexed-map [0 0])
        site)))

(defn get-north-site 
  "Get the site 'north' of the given site"
  [game-map site]
  (get-at game-map (:x site) (dec (:y site))))

(defn get-south-site
  "Get the site 'south' of the given site"
  [game-map site]
  (get-at game-map (:x site) (inc (:y site))))

(defn get-east-site
  "Get the site 'east' of the given site"
  [game-map site]
  (get-at game-map (inc (:x site)) (:y site)))

(defn get-west-site 
  "Get the site 'west' of the given site"
  [game-map site]
  (get-at game-map (dec (:x site)) (:y site)))

(defn is-owner 
  "Checks if an id is the owner of a site"
  [my-id site]
  (= (:owner site) my-id))

(defn is-not-owner
  "Checks if an id is not the owner of a site"
  [my-id site]
  (not (is-owner my-id site)))

(defn distance-generic 
  "A generic function for distance calculation"
  [map-size f a b]
  (let [dist1 (mod (- a b) map-size)
        dist2 (mod (- b a) map-size)]
      (f dist1 dist2)))

(defn distance-x
  "Calculate the distance between two sites along the x axis"
  [map-size a b]
  (distance-generic map-size min (:x a) (:x b)))

(defn direction-x 
  "Calculate the direction a strength must travel between two sites (a to b) along the x axis"
  [map-size a b]
  (distance-generic map-size #(if (< %1 %2) :west :east) (:x a) (:x b)))

(defn distance-y
  "Calculate the distance between two sites along the y axis"
  [map-size a b]
  (distance-generic map-size min (:y a) (:y b)))

(defn direction-y 
  "Calculate the direction a strength must travel between two sites (a to b) along the x axis"
  [map-size a b]
  (distance-generic map-size #(if (< %1 %2) :north :south) (:y a) (:y b)))

(defn distance 
  "Calculate the distance between two sites"
  [map-size a b]
  (let [dist-x (distance-x map-size a b)
        dist-y (distance-y map-size a b)]
   (+ dist-x dist-y)))

(defn get-val-distance
  "Gets the value-distance to various sites"
  [my-id map-size site other-site]
  (let [new-dist (distance map-size site other-site)]
    (if (and (not= (:owner other-site) my-id) (not= (:owner other-site) 0))
      (max 0 (- new-dist 4))
      new-dist)))

(defn get-closest-site 
  "Get the site closest to the current site. Expensive"
  [my-id game-map site]
  (let [map-size (count game-map)]
    (->> @filtered-not-me-map
         (reduce (fn [[r v] x]
                  (let [new-dist (get-val-distance my-id map-size site x)] 
                    (if (< v new-dist) [r v] [x new-dist]))) 
                [{} 1000])
         (first))))

(defn get-closest-site-direction 
  "Get the direction of the closet site"
  [my-id game-map site]
  (let [closest-site (get-closest-site my-id game-map site)
        map-size (count game-map)
        dist-x (distance-x map-size site closest-site)
        dist-y (distance-y map-size site closest-site)]
    (cond
      (zero? dist-y) [site (direction-x map-size site closest-site)]
      (zero? dist-x) [site (direction-y map-size site closest-site)]
      :else [site (rand-nth [(direction-x map-size site closest-site) (direction-y map-size site closest-site)])])))

(defn site-value 
  "Calculated the value of a site based on it's strength and production"
  [site]
  (let [str-value (- max-str (:strength site))
        prod-value (* 4 (:production site))]
    (+ str-value prod-value)))

(defn get-highest-value-site
  "Given a list of sites finds the one with the highest production"
  [my-id sites]
  (let [filtered (->> sites
                      (filter #(is-not-owner my-id %)))]
      (if (empty? filtered)
        nil
        (reduce #(if (< (site-value %1) (site-value %2)) %2 %1) filtered))))

(defn get-strongest-friend-site 
  "Gets the strongs nearby friend site"
  [my-id sites]
  (let [filtered (->> sites
                    (filter #(is-owner my-id %)))]
    (if (empty? filtered)
      nil
      (reduce #(if (< (:strength %1) (:strength %2)) %2 %1) filtered))))

(defn get-lowest-direction
  "Given a cell that I own figure out the closest cell next to me that is not owned by me. Send that way."
  [my-id game-map site]
  (let [north-site (get-north-site game-map site)
          east-site (get-east-site game-map site)
          south-site (get-south-site game-map site)
          west-site (get-west-site game-map site)
          strongest-friend-site (get-strongest-friend-site my-id [north-site east-site south-site west-site])
          weak-site (get-highest-value-site my-id [north-site east-site south-site west-site])]
    (if (and (< (:strength site) (* 6 (:production site))) 
             (<= (:strength site) max-str) 
             (not (nil? strongest-friend-site)) 
             (< (+ (:strength site) (:strength strongest-friend-site)) max-str))
      [site :still]
    ;;TODO: Adjecent sites fn
      (if (nil? weak-site)
        (get-closest-site-direction my-id game-map site)
        (if (and (<= (:strength site) (:strength weak-site)) (not (nil? strongest-friend-site)) (< (+ (:strength site) (:strength strongest-friend-site)) max-str))
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
                      (filter #(is-owner my-id %)))]
    (->> my-sites
      (map #(get-lowest-direction my-id game-map %)))))

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (do
          (reset! indexed-map (->> game-map flatten (reduce (fn [r v] (assoc r [(:x v) (:y v)] v) ) {}))) 
          (reset! filtered-not-me-map (->> game-map flatten (filter #(is-not-owner my-id %))))
          (io/send-moves! (certain-moves my-id game-map)))))))
