(ns pluralex.core
  (:require [clojure.string :as str]))

(def ^:private unchanging-words
  (map str '(children
             deer
             information
             equipment
             fish
             jeans
             men
             mice
             money
             moose
             news
             pasta
             people
             police
             quinoa
             rice
             sheep
             spaghetti
             weaponry
             women)))

(def ^:private irregular-words
  {"axis" "axes"
   "buffalo" "buffaloes"
   "bus" "busses"
   "child" "children"
   "goose" "geese"
   "matrix" "matrices"
   "mouse" "mice"
   "ox" "oxen"
   "tomato" "tomatoes"
   "vertex" "vertices"})

(defn- add-plural
  [word trim plural]
  (-> word (subs 0 (-> word count (- trim))) (str plural)))

(defn pluralize
  [w]
  (let [w' (name w)]
    (cond
      (some #{w'} unchanging-words) w'
      (contains? irregular-words w') (irregular-words w')
      (str/ends-with? w' "ch") (add-plural w' 0 "es")
      (str/ends-with? w' "s") (add-plural w' 0 "es")
      (str/ends-with? w' "x") (add-plural w' 0 "es")
      (str/ends-with? w' "zz") (add-plural w' 0 "es")
      (str/ends-with? w' "z") (add-plural w' 0 "zes")
      (str/ends-with? w' "man") (add-plural w' 2 "en")
      (str/ends-with? w' "y") (add-plural w' 1 "ies")
      :else
      (add-plural w' 0 "s"))))

