(ns Clojite_v6
  (:require [game]
            [io])
  (:gen-class))


;; Changes
;; ===
;; 1. If a stie is next to an empty site send whatever you have

(def bot-name "Clojite v6")
(def max-str 255)

(defn get-at 
  "Get site info at a specific x and y"
  [game-state x y]
  (let [game-size (:map-size game-state)
        real-x (mod x game-size)
        real-y (mod y game-size)
        indexed-map (:indexed-map game-state)
        site (get indexed-map [real-x real-y])]
    (if (nil? site)
        (get indexed-map [0 0])
        site)))

(defn get-north-site 
  "Get the site 'north' of the given site"
  [game-state site]
  (get-at game-state (:x site) (dec (:y site))))

(defn get-south-site
  "Get the site 'south' of the given site"
  [game-state site]
  (get-at game-state (:x site) (inc (:y site))))

(defn get-east-site
  "Get the site 'east' of the given site"
  [game-state site]
  (get-at game-state (inc (:x site)) (:y site)))

(defn get-west-site 
  "Get the site 'west' of the given site"
  [game-state site]
  (get-at game-state (dec (:x site)) (:y site)))

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

(defn site-distance-value 
  "Calculated the value of a site based on it's strength and production"
  [my-id map-size a b]
  (let [enemy (if (and (not= (:owner b) my-id) (not= (:owner b) 0)) 10 0)
        prod-value (* 4 (:production b))
        distance (distance map-size a b)]
(- distance enemy)))

(defn get-val-distance
  "Gets the value-distance to various sites"
  [my-id map-size site other-site]
  (let [new-dist (site-distance-value my-id map-size site other-site)]
    new-dist))

(defn get-closest-site 
  "Get the site closest to the current site. Expensive"
  [my-id game-state site]
  (let [map-size (:map-size game-state)]
    (->> (:filtered-not-me-map game-state)
         (reduce (fn [[r v] x]
                  (let [new-dist (get-val-distance my-id map-size site x)] 
                    (if (< v new-dist) [r v] [x new-dist]))) 
                [{} 1000])
         (first))))


(defn site-value 
  "Calculated the value of a site based on it's strength and production"
  [my-id site]
  (let [enemy (if (and (not= (:owner site) my-id) (not= (:owner site) 0)) 255 0)
        str-value (- max-str (:strength site))
        prod-value (* 6 (:production site))]
    (+ enemy str-value prod-value)))


(defn get-closest-site-direction 
  "Get the direction of the closet site"
  [my-id game-state site]
  (let [closest-site (get-closest-site my-id game-state site)
        map-size (:map-size game-state)
        dist-x (distance-x map-size site closest-site)
        dist-y (distance-y map-size site closest-site)]
    (cond
      (zero? dist-y) [site (direction-x map-size site closest-site)]
      (zero? dist-x) [site (direction-y map-size site closest-site)]
      :else [site (rand-nth [(direction-x map-size site closest-site) (direction-y map-size site closest-site)])])))


(defn get-highest-value-site
  "Given a list of sites finds the one with the highest production"
  [my-id sites]
  (let [filtered (->> sites (filter #(is-not-owner my-id %)))]
      (if (empty? filtered) nil
        (reduce #(if (< (site-value my-id %1) (site-value my-id %2)) %2 %1) filtered))))

(defn get-strongest-friend-site 
  "Gets the strongs nearby friend site"
  [my-id sites]
  (let [filtered (->> sites
                    (filter #(is-owner my-id %)))]
    (if (empty? filtered)
      nil
      (reduce #(if (< (:strength %1) (:strength %2)) %2 %1) filtered))))

(defn has-foreign-sites [game-state site]
   (let [north-site (get-north-site game-state site)
          east-site (get-east-site game-state site)
          south-site (get-south-site game-state site)
          west-site (get-west-site game-state site)
          weak-site (get-highest-value-site (:my-id game-state) [north-site east-site south-site west-site])]
    (not (nil? weak-site))))

(defn other-weak-site [game-state site]
   (let [north-site (get-north-site game-state site)
          east-site (get-east-site game-state site)
          south-site (get-south-site game-state site)
          west-site (get-west-site game-state site)
          weak-site (get-highest-value-site (:my-id game-state) [north-site east-site south-site west-site])]
    weak-site))

(defn is-enemy-site [my-id site]
    (and (not= (:owner site) my-id) (not= (:owner site) 0)))


(defn get-lowest-direction
  "Given a cell that I own figure out the closest cell next to me that is not owned by me. Send that way."
  [game-state site]
  (let [north-site (get-north-site game-state site)
          east-site (get-east-site game-state site)
          south-site (get-south-site game-state site)
          west-site (get-west-site game-state site)
          strongest-friend-site (get-strongest-friend-site (:my-id game-state) [north-site east-site south-site west-site])
          weak-site (get-highest-value-site (:my-id game-state) [north-site east-site south-site west-site])]
    (if (and (< (:strength site) (* 6 (:production site))) 
             (<= (:strength site) max-str) 
             (not (nil? strongest-friend-site))
             (not= (:strength weak-site) 0)
             (< (+ (:strength site) (:strength strongest-friend-site)) max-str))
      [site :still]
    ;;TODO: Adjecent sites fn
      (if (nil? weak-site)
        (get-closest-site-direction (:my-id game-state) game-state site)
        (if (and (<= (:strength site) (:strength weak-site)) 
                 (not (nil? strongest-friend-site)) 
                 (< (+ (:strength site) (:strength strongest-friend-site)) max-str))
         (if (or (> (:strength site) (:strength strongest-friend-site))
                  (not (has-foreign-sites game-state strongest-friend-site)))
           [site :still]
           (cond
              (= strongest-friend-site north-site)  [site :north]
              (= strongest-friend-site south-site)  [site :south]
              (= strongest-friend-site east-site)  [site :east]
              (= strongest-friend-site west-site)  [site :west]
              :else [site :north]))
         (cond
           (= weak-site north-site)  [site :north]
           (= weak-site south-site)  [site :south]
           (= weak-site east-site)  [site :east]
           (= weak-site west-site)  [site :west]
           :else [site :north]))))))

(defn get-lowest-direction2
  "Given a cell that I own figure out the closest cell next to me that is not owned by me. Send that way."
  [game-state site]
  (let [north-site (get-north-site game-state site)
          east-site (get-east-site game-state site)
          south-site (get-south-site game-state site)
          west-site (get-west-site game-state site)
          strongest-friend-site (get-strongest-friend-site (:my-id game-state) [north-site east-site south-site west-site])
          weak-site (get-highest-value-site (:my-id game-state) [north-site east-site south-site west-site])]
    (if (and (< (:strength site) (* 6 (:production site))) 
             (<= (:strength site) max-str) 
             (not (nil? strongest-friend-site))
             (not= (:strength weak-site) 0)
             (< (+ (:strength site) (:strength strongest-friend-site)) max-str))
      [site :still]
      (if (nil? weak-site)
        (get-closest-site-direction (:my-id game-state) game-state site)
        (if (and (not (nil? strongest-friend-site))
                 (has-foreign-sites game-state strongest-friend-site)
                 (< (:production weak-site) (:production (other-weak-site game-state strongest-friend-site)))
                 (< (+ (:strength site) (:strength strongest-friend-site)) max-str)
                 (< (:strength (other-weak-site game-state strongest-friend-site))  (+ (:strength site) (:strength strongest-friend-site))))
          (cond
            (= strongest-friend-site north-site)  [site :north]
            (= strongest-friend-site south-site)  [site :south]
            (= strongest-friend-site east-site)  [site :east]
            (= strongest-friend-site west-site)  [site :west]
            :else [site :north])
         (let [nn-site (get-north-site game-state north-site)
               ee-site (get-east-site game-state east-site)
               ss-site (get-south-site game-state south-site)
               ww-site (get-west-site game-state west-site)]
          (if (<= (:strength site) (:strength weak-site))
            [site :still]
            (cond 
              (is-enemy-site (:my-id game-state) nn-site) [site :north]
              (is-enemy-site (:my-id game-state) ee-site) [site :east]
              (is-enemy-site (:my-id game-state) ss-site) [site :south]
              (is-enemy-site (:my-id game-state) ww-site) [site :west]
              (= weak-site north-site)  [site :north]
              (= weak-site south-site)  [site :south]
              (= weak-site east-site)  [site :east]
              (= weak-site west-site)  [site :west]
:else [site :north]))))))))

(defn certain-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [game-state]
  (let [my-sites (->> (:game-map game-state)
                      flatten
                      (filter #(is-owner (:my-id game-state) %)))]
    (->> my-sites
      (map #(get-lowest-direction2 game-state %)))))

(defn generate-game-state [my-id game-map]
  {
    :my-id my-id
    :game-map game-map
    :indexed-map (->> game-map flatten (reduce (fn [r v] (assoc r [(:x v) (:y v)] v) ) {}))
    :filtered-not-me-map (->> game-map flatten (filter #(is-not-owner my-id %)))
    :map-size (count game-map)})  
    
    

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (do
          (io/send-moves! (certain-moves (generate-game-state my-id game-map))))))))
