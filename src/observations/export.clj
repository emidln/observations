(ns observations.export
  (:refer-clojure :exclude [==])
  (:require [pldb.logic :as dblogic])
  (:require [observations.dbutils :as schema])
  (:use [clojure.tools.logging :only (info error debug warn)]))

(defn sql-row-values [args row]
  (map-indexed (fn [i val]
                 (let [tag (str  (:tag (meta (nth args i "Hash"))))]
                   (cond
                    (= tag "Hash")
                    (str "\\x" val)
                    true
                    val
                    )))
               row))

(defn dump-observation-as-insert [odef scopemap]
  ;; time model should be replaced by scope options
  (let [scoped (:scope (:options odef))
        args (concat scoped (:args odef))
        scope-args (select-keys scopemap scoped)
        tname (name (:relation odef))]
    (doseq [row (dblogic/facts-for  (keyword (name (:relation odef))))]
      (let [row-values (sql-row-values args row)]
        (schema/print-insert tname (zipmap args
                                           (concat scope-args row)))))))



    
  

