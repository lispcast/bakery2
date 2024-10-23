(ns bakery.robot
  (:require [clojure.string :as string]))

(def initial-prep-stock {:corn-starch 300
                         :flour       300
                         :sugar       300
                         :coconut-oil 300
                         :almond-milk 300
                         :cocoa       300
                         :lemon       300
                         :baking-powder 300})

(def initial-state {:bowls {:wet {:state :empty
                                  :ingredients {}}
                            :dry {:state :empty
                                  :ingredients {}}}
                    :pan {:state :empty
                          :ingredients {}}
                    :hand nil
                    :cooling-rack {}
                    :location :prep-area
                    :loaded {}
                    :prep-stock initial-prep-stock
                    :remaining-orders {}
                    :problems []})

(defonce state (atom initial-state))

(def _ingredients        #{:corn-starch :flour :almond-milk :sugar :coconut-oil :cocoa :lemon :baking-powder})
(def _fridge-ingredients #{:almond-milk :lemon})
(def _pantry-ingredients #{:corn-starch :flour :sugar :cocoa :coconut-oil :baking-powder})
(def _grabbables         #{:cup :lemon})
(def _squeezables        #{:lemon})
(def _scoopables         #{:flour :almond-milk :sugar :coconut-oil :cocoa :ganache :corn-starch :baking-powder})
(def _dry-ingredients    #{:corn-starch :flour :cocoa :baking-powder})
(def _wet-ingredients    #{:almond-milk :sugar :coconut-oil :ganache :lemon-juice})
(def _locations          #{:prep-area :fridge :pantry})


(defn- from-cup [x]
  (let [c "cup-of-"]
    (when (and x
               (= (seq c) (take (count c) (name x))))
      (keyword (.substring (name x) (count c))))))

(defn- from-cup [x]
  (when (and (vector? x)
             (= :cup-of (first x)))
    (second x)))

;; just to get a clean state again

(defn start-over
  "Clean up the bakery, empty hands, clean out the bowl, get a new pan. Good when you mess up and need a fresh start."
  []
  (reset! state initial-state)
  :ok)

(defn error [& args]
  (apply println args)
  :error)

(defn grab
  "Grab an item from the bakery (:cup)."
  [item]
  (cond
    (not= (-> state deref :location) :prep-area)
    (error "I can only grab things in the prep area. I am at the" (-> state deref :location))

    (not (contains? _grabbables item))
    (error "I cannot grab" item)

    (not (or (= :cup item)
             (pos? (-> state deref :prep-stock (get item 0)))))
    (error item "is not available at the prep area")

    (-> state deref :hand)
    (error "I already have" (name (-> state deref :hand)) "in my hand.")

    :else
    (do
      (swap! state (fn [state]
                     (cond-> state
                       true (assoc :hand item)
                       (not= :cup item) (update-in [:prep-stock item] dec))))
      :ok)))

(defn scoop
  "If you are holding the cup, scoop up something into your measuring cup
   (milk, flour, or sugar)."
  [ingredient]
  (cond
    (not= (-> state deref :location) :prep-area)
    (error "I can only scoop things in the prep area. I am at the" (-> state deref :location))

    (not= (-> state deref :hand) :cup)
    (error "I need to have the cup in my hand to scoop.")

    (not (contains? _scoopables ingredient))
    (error ingredient "is not scoopable.")

    (not (pos? (-> state deref :prep-stock (get ingredient 0))))
    (error ingredient "is not available at the prep area")

    :else
    (do
      (swap! state (fn [state]
                     (-> state
                         (assoc :hand [:cup-of ingredient])
                         (update-in [:prep-stock ingredient] dec))))
      :ok)))

(defn release
  "Release whatever you are holding in your hand."
  []
  (cond
    (nil? (-> state deref :hand))
    (error "I am not holding anything.")

    (not= (-> state deref :location) :prep-area)
    (error "Please only release things in the prep area.")

    :else
    (do
      (swap! state (fn [state]
                     (cond-> state
                     ;; add the ingredient back to the prep stock
                       (from-cup (-> state :hand)) (update-in [:prep-stock (from-cup (-> state :hand))] inc)
                     ;; add the ingredient back to the prep stock
                       (contains? _ingredients (-> state :hand)) (update-in [:prep-stock (-> state :hand)] inc)
                     ;; empty hand
                       true (assoc :hand nil))))
      :ok)))

(defn- from-squeeze [item]
  (cond
    (= :lemon item)
    :lemon-juice))

(defn squeeze
  "Squeeze the lemon you are holding to juice it."
  [bowl]
  (let [squeeze-ingredient (from-squeeze (-> state deref :hand))]
    (cond
      (not= (-> state deref :location) :prep-area)
      (error "I can only add things to the bowl in the prep area. I am at the" (-> state deref :location))

      (nil? (-> state deref :hand))
      (error "I am not holding anything.")

      (not (contains? _squeezables (-> state deref :hand)))
      (error "I am holding" (-> state deref :hand) ", which is not squeezable.")

      (and (= bowl :wet)
           (not (contains? _wet-ingredients squeeze-ingredient)))
      (error squeeze-ingredient "is not a wet ingredient. Don't add it to the wet bowl.")

      (and (= bowl :dry)
           (not (contains? _dry-ingredients squeeze-ingredient)))
      (error squeeze-ingredient "is not a dry ingredient. Don't add it to the dry bowl.")

      :else
      (do
        (swap! state (fn [state]
                       (-> state
                           (update-in [:bowls bowl :ingredients squeeze-ingredient] (fnil inc 0))
                           (assoc-in [:bowls bowl :state] :unmixed)
                           (assoc-in [:hand] :squeezed-lemon))))
        :ok))))

(defn dump
  "Add what is in your hand to the bowl."
  [bowl]
  (let [cup-ingredient (from-cup (-> state deref :hand))]
    (cond
      (not= (-> state deref :location) :prep-area)
      (error "I can only add things to the bowl in the prep area. I am at the" (-> state deref :location))

      (nil? (-> state deref :hand))
      (error "I am not holding anything.")

      (= :cup (-> state deref :hand))
      (error "My cup is empty.")

      (and (= bowl :wet)
           (not (contains? _wet-ingredients cup-ingredient)))
      (error cup-ingredient "is not a wet ingredient. Don't add it to the wet bowl.")

      (and (= bowl :dry)
           (not (contains? _dry-ingredients cup-ingredient)))
      (error cup-ingredient "is not a dry ingredient. Don't add it to the dry bowl.")

      :else
      (do
        (swap! state (fn [state]
                       (-> state
                           (update-in [:bowls bowl :ingredients cup-ingredient] (fnil inc 0))
                           (assoc-in [:bowls bowl :state] :unmixed)
                           (assoc-in [:hand] :cup))))
        :ok))))

(defn mix-bowl
  "Mix the contents of the bowl."
  [bowl]
  (cond
    (not= (-> state deref :location) :prep-area)
    (error "I can only mix the bowl when I'm in the prep area. I'm at the" (-> state deref :location))

    (= :empty (-> state deref :bowls bowl :state))
    (error "The" (name bowl) "bowl is empty.")

    (= :wet bowl)
    (do
      (swap! state (fn [state]
                     (let [ingredients (-> state :bowls :wet :ingredients)]
                       (if (= {:coconut-oil 2
                               :cocoa 2
                               :sugar 1} ingredients)
                         (assoc-in state [:bowls :wet :ingredients] {:ganache 2})
                         (assoc-in state [:bowls :wet :state] :mixed)))))
      :ok)

    (= :dry bowl)
    (do
      (swap! state (fn [state]
                     (assoc-in state [:bowls :dry :state] :mixed)))
      :ok)))

(defn add-contents [a b]
  (if (or (nil? a) (map? a))
    (merge-with + a b)
    :randomness))

(comment

  (add-contents
   {:flour 1
    :corn-starch 1}
   {:coconut-oil 1
    :sugar 1}))


(defn pour-bowl
  "Pour the contents fo the bowl into the destination (:wet, :dry, :pan)."
  [bowl destination]
  (cond
    (not= (-> state deref :location) :prep-area)
    (error "I can only pour the bowl when I'm in the prep area. I'm at the" (-> state deref :location))

    (= :empty (-> state deref :bowls bowl :state))
    (error "The" (name bowl) "bowl is empty.")

    (= :unmixed (-> state deref :bowls bowl :state))
    (error "The" (name bowl) "bowl is unmixed, you should mix it first (using the mix-bowl function).")

    (= bowl destination)
    (error "You cannot pour into the same bowl.")

    (= :pan destination)
    (cond
      (not= :empty (-> state deref :pan :state))
      (error "The pan is not empty. You should only pour into an empty pan.")

      :else
      (do
        (swap! state (fn [state]
                       (-> state
                           (assoc-in [:pan] (-> state :bowls bowl))
                           (assoc-in [:bowls bowl] {:state :empty :ingredients {}}))))
        :ok))

    :else ;; destination is a bowl
    (cond
      (= :unmixed (-> state deref :bowls destination :state))
      (error "The" (name destination) "bowl is unmixed, you should mix it first (using the mix-bowl function).")

      :else
      (do
        (swap! state (fn [state]
                       (-> state
                           (update-in [:bowls destination :ingredients] add-contents (-> state :bowls bowl :ingredients))
                           (assoc-in [:bowls destination :state] :unmixed)
                           (assoc-in [:bowls bowl] {:state :empty :ingredients {}}))))
        :ok))))

(comment
  ;; bake cookies
  (do
    (start-over)
    (grab :cup)
    (scoop :flour)
    (dump :dry)
    (scoop :corn-starch)
    (dump :dry)
    (scoop :sugar)
    (dump :wet)
    (scoop :coconut-oil)
    (dump :wet)
    (mix-bowl :dry)
    (mix-bowl :wet)
    (pour-bowl :wet :dry)

    (mix-bowl :dry)
    (pour-bowl :dry :pan)
    (bake-pan 30)
    (cool-pan)

    (status)))

(def recipes
  {:cookies {:ingredients {:flour 1
                           :corn-starch 1
                           :sugar 1
                           :coconut-oil 1}
             :time 30}

   :cake {:ingredients {:flour 2
                        :sugar 1
                        :almond-milk 1
                        :baking-powder 1}
          :time 25}

   :brownies {:ingredients {:flour 2
                            :almond-milk 1
                            :sugar 1
                            :cocoa 1}
              :time 35}

   :lemon-squares {:ingredients {:flour 1
                                 :corn-starch 2
                                 :lemon 10
                                 :sugar 2}
                   :time 20}})

(def recipes-by-ingredients
  (into {} (for [[name {:keys [ingredients time]}] recipes]
             [ingredients {:name name :time time}])))

(defn- bake [pan-ingredients bake-time]
  (let [{:keys [name time]} (get recipes-by-ingredients pan-ingredients)]
    (cond
      (nil? time)
      :randomness

      (= time time)
      name

      (< bake-time time)
      :mushy-mess

      (> bake-time time)
      :burned-mess

      :else
      (throw (ex-info "This should not happen." {})))))

(defn bake-pan
  "Put the pan in the oven and bake it for so many minutes."
  [minutes]
  (cond
    (not (number? minutes))
    (error "I need a number of minutes to bake. You gave me a" (type minutes) "(" minutes ")")

    (not= :prep-area (-> state deref :location))
    (error "I can only bake at the prep area. I am at the" (-> state deref :location))

    (= :empty (-> state deref :pan :state))
    (error "There's nothing in the pan!"
           "Use (pour-into-pan) to transfer from the bowl to the pan.")

    :else
    (do
      (println "Baking" minutes "minutes. . .")
      (swap! state (fn [state]
                     (-> state
                         (update-in [:pan :ingredients] bake minutes)
                         (assoc-in [:pan :state] :baked))))
      (println "Done!")
      (println "The result is" (name (-> state deref :pan :ingredients)))
      :ok)))

(defn cool-pan
  "Remove the pan from the oven and put it in the cooling rack. Also grab a new pan to make something new! Returns the name of the cooling rack where the pan was placed."
  []
  (cond
    (not= :prep-area (-> state deref :location))
    (error "I can only cool the pan at the prep area. I am at the" (-> state deref :location))

    (= :empty (-> state deref :pan :state))
    (error "There's nothing in the pan! Use (pour-into-pan) to transfer from the bowl to the pan.")

    (not= :baked (-> state deref :pan :state))
    (error "You don't need to cool the pan. It's not baked yet. Use (bake-pan <minutes>) to bake it.")

    :else
    (let [r (gensym "cooling-rack-")]
      (swap! state (fn [state]
                     (-> state
                         (assoc-in [:cooling-rack r] (-> state :pan :ingredients))
                         (assoc-in [:pan] {:state :empty :ingredients {}}))))
      (println "Cooling pan!")
      (println "I now have a fresh pan.")
      r)))

(defn status
  "Look around and report the status of the bakery."
  []
  (let [l (for [[k v] (-> state deref :loaded)
                :when (pos? v)]
            (if (_scoopables k)
              (if (= 1 v)
                (str v " cup of " (name k))
                (str v " cups of " (name k)))
              (if (= 1 v)
                (str v " " (name k))
                (str v " " (name k) "s"))))]
    (print "Loaded up: ")
    (if (seq l)
      (println (string/join ", " l))
      (println "nothing")))

  (println)

  (print "Location: ")
  (println (string/replace (name (-> state deref :location)) "-" " "))

  (println)

  (when (= (-> state deref :location) :fridge)
    (print "Available ingredients: ")
    (if (seq _fridge-ingredients)
      (println (string/join ", " (map name _fridge-ingredients)))
      (println "none")))

  (when (= (-> state deref :location) :pantry)
    (print "Available ingredients: ")
    (if (seq _pantry-ingredients)
      (println (string/join ", " (map name _pantry-ingredients)))
      (println "none")))

  (when (= (-> state deref :location) :prep-area)
    (let [i (for [[k v] (-> state deref :prep-stock)
                  :when (pos? v)]
              (if (_scoopables k)
                (if (= 1 v)
                  (str v " cup of " (name k))
                  (str v " cups of " (name k)))
                (if (= 1 v)
                  (str v " " (name k))
                  (str v " " (name k) "s"))))]
      (print "Available ingredients: ")
      (if (seq i)
        (println (string/join ", " i))
        (println "none")))

    (println)

    (print "In hand: ")
    (if (-> state deref :hand)
      (println (string/replace (name (-> state deref :hand)) "-" " "))
      (println "nothing"))

    (doseq [bowl [:dry :wet]]
      (println)
      (print (string/capitalize (name bowl)) "bowl is" (-> state deref :bowls bowl :state name))
      (when (not= :empty (-> state deref :bowls bowl :state))
        (print ", containing: ")
        (println (string/join ", " (for [[k v] (-> state deref :bowls bowl :ingredients)
                                         :when (pos? v)]
                                     (if (_scoopables k)
                                       (if (= 1 v)
                                         (str v " cup of " (name k))
                                         (str v " cups of " (name k)))
                                       (if (= 1 v)
                                         (str v " " (name k))
                                         (str v " " (name k) "s"))))))))

    (println)
    (println)

    (print "In pan")
    (if (= :baked (-> state deref :pan :state))
      (print " (in oven)")
      (print ""))
    (print ": ")
    (cond
      (= :empty (-> state deref :pan :state))
      (println "nothing.")

      (keyword? (-> state deref :pan :ingredients))
      (println (name (-> state deref :pan :ingredients)))

      :else
      (println (string/join ", " (for [[k v] (-> state deref :pan :ingredients)]
                                   (if (_scoopables k)
                                     (str v " cups of " (name k))
                                     (str v " " (name k) "s")))))))
  (println)

  (print "On the cooling rack: ")
  (if (seq (-> state deref :cooling-rack))
    (println (string/join ", " (for [[k v] (-> state deref :cooling-rack)]
                                 (str (name v) " in " (string/replace (name k) "-" " ")))))
    (println "nothing"))
  :ok)

(defn go-to
  "Go to the given location.
   Possible locations are :prep-area, :fridge, and :pantry."
  [place]
  (cond
    (not (_locations place))
    (error "I do not know where" place "is.")

    (= place (-> state deref :location))
    (do
      (println "I am already in" place)
      :ok)

    :else
    (do
      (println "Going to" place)
      (swap! state (fn [state]
                     (assoc state :location place)))
      :ok)))

(defn load-up
  "Load an ingredient from storage to take to the prep area."
  [ingredient]
  (cond
    (= :fridge (-> state deref :location))
    (if (_fridge-ingredients ingredient)
      (do
        (swap! state update-in [:loaded ingredient] (fnil inc 0))
        :ok)
      (error ingredient "is not available at the fridge"))

    (= :pantry (-> state deref :location))
    (if (_pantry-ingredients ingredient)
      (do
        (swap! state update-in [:loaded ingredient] (fnil inc 0))
        :ok)
      (error ingredient "is not available at the pantry"))

    :else
    (error "I can only load up at the fridge and pantry. I'm at the" (-> state deref :location))))

(defn unload
  "Unload an ingredient in the prep area."
  [ingredient]
  (cond
    (not= :prep-area (-> state deref :location))
    (error "I can only unload at the prep area")

    (zero? (-> state deref :loaded (get ingredient 0)))
    (error "I don't have any" ingredient "loaded")

    :else
    (do
      (swap! state (fn [state]
                     (-> state
                         (update-in [:loaded ingredient] dec)
                         (update-in [:prep-stock ingredient] inc))))
      :ok)))

(def orderid (atom (rand-int 10000)))

(def streets ["Robot Ln", "Cyber Dr", "Electro Pkwy", "Servo St", "Android Ave", "Replicant Rd", "Actuator Hwy"])

(defn- get-morning-orders*
  "Get a new list of baking orders."
  [items n]
  (let [orders (for [_ (range 10)]
                 {:orderid (swap! orderid inc)
                  :address (str (rand-int 1000) " " (rand-nth streets))
                  :items (into {} (for [x items
                                        :let [n (rand-int n)]
                                        :when (pos? n)]
                                    [x n]))})
        orders-by-id (into {} (map (juxt :orderid identity)) orders)]
    (swap! state (fn [state]
                   (-> state
                       (assoc :remaining-orders orders-by-id)
                       (assoc :over-baked {}))))
    (dorun orders)
    orders))

(defn get-morning-orders
  "Get a new list of baking orders."
  []
  (get-morning-orders* [:cake :cookies] 25))

(defn get-morning-orders-day3
  "Get a new list of baking orders including brownies and lemon bars."
  []
  (get-morning-orders* [:cake :cookies :brownies :lemon-squares] 25))

(defn get-morning-orders-count
  "Get a new list of baking orders including brownies."
  [n]
  (get-morning-orders* [:cake :cookies :brownies :lemon-squares] n))

(defn order-items [order]
  (reduce + 0 (vals (:items order))))

(defn orders-items [orders]
  (reduce + 0 (map order-items (vals orders))))

(defn complete? [orders]
  (zero? (orders-items orders)))

(defn- deliver-1-*
  [state order-id rack-id]
  (let [contents (-> state :cooling-rack (get rack-id))
        remaining (-> state :remaining-orders (get-in [order-id :items contents]))]
    (if (pos? remaining)
      (update-in state [:remaining-orders order-id :items contents] dec)
      (update-in state [:problems         order-id        contents] inc))))

(defn delivery
  "Notify the delivery bot that something is ready to deliver"
  [receipt]
  (swap! state (fn [state]
                 (reduce (fn [state rack-id]
                           (deliver-1-* state (:orderid receipt) rack-id))
                         state
                         (:rackids receipt))))
  (doseq [[product count] (-> state deref :problems (get (:order-id receipt)))]
    (println "Baked too many" product "for order" (:order-id receipt) "by" count "."))
  (when (zero? (order-items (-> state deref :remaining-orders (get (:order-id receipt)))))
    (println "Order" (:order-id receipt) "is complete!"))
  (when (complete? (-> state deref :remaining-orders))
    (println "All morning orders are complete!"))
  :ok)

(defn bakery-help
  "Print out some helpful text to remember the available commands."
  []
  (println "Welcome to the bakery!")
  (println)
  (println "Available commands are: ")
  (println "grab               Pick something up.")
  (println "release            Release whatever you are holding.")
  (println "scoop              If you are holding the cup, fill it with an ingredient.")
  (println "dump               Add the ingredient you are holding to a bowl.")
  (println "squeeze            Squeeze the lemon you are holding to juice it.")
  (println "mix-bowl           Mix the ingredients in the bowl.")
  (println "pour-into-pan      Pour the contents of the bowl into the pan.")
  (println "bake-pan           Put the pan in the oven for a certain number of minutes.")
  (println "cool-pan           After baking, put the pan on the cooling racks to cool.")
  (println "                     Returns the id of the cooling rack.")
  (println "status             Print out the status of the bakery.")
  (println "start-over         If you get lost, this command will reset the bakery.")
  (println "go-to              Go to the given location.")
  (println "                     Possible locations are :prep-area, :fridge, and :pantry.")
  (println "load-up            Load an ingredient from storage to take to the prep area.")
  (println "unload             Unload an ingredient in the prep area.")
  (println "get-morning-orders Get a new list of baking orders.")
  (println "delivery           Notify the delivery bot that something is ready to deliver.")
  (println)
  (println "bakery-help        Print out this message.")

  'welcome)

(bakery-help)