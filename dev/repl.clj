(ns repl
  (:require [rebel-readline.clojure.main :as rebel]))

(defn -main [& args]
  (require '[clojure.repl :refer :all]
           '[bakery.robot :refer :all])
  (apply rebel/-main args))