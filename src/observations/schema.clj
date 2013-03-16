(ns observations.schema
    (:use observations.dbutils))

;; should be defmulti
(defn- arg-as-coldef [arg]
  (let [tag (str (:tag (meta arg)))
        data_type (cond
                   (or (= tag "String")
                       (= tag "Filename")
                       (= tag "Directory")
                       (= tag "Pathname")
                       (= tag "Path")
                       (= tag "RegistryKey"))
                   "VARCHAR"
                   (= tag "Hash")
                   "BYTEA"
                   (= tag "Timestamp")
                   "NUMERIC"
                   (= tag "Integer")
                   "NUMERIC"
                   (= tag "Boolean")
                   "BOOLEAN"
                   (= tag "Address")
                   "NUMERIC"
                   (= tag "Date")
                   "TIMESTAMP WITH TIME ZONE"
                   (= tag "List")
                   "VARCHAR[]"
                   (= tag "IPAddress")
                   "INET"
                   (= tag "Number")
                   "NUMERIC"
                   true
                   "VARCHAR"
                   )]
    [arg data_type (when (:required (meta arg)) " NOT NULL")]))

(defn print-observation-table [obs]
  (let [tname (name (relation obs))
        time-model (:time-model (options obs))]
    (print
     (as-comment (:doc (meta (ns-resolve (find-ns 'threatbrain.engine) (relation obs))))))
    (print-create-table tname (concat (cond
                                        (= time-model :sample)
                                        [["sample" "VARCHAR" "NOT NULL"]]
                                        (= time-model :mutating)
                                        [["ts" "TIMESTAMP" "NOT NULL"]]
                                        (= time-model :permanent)
                                        nil)
                                      (map arg-as-coldef (args obs)))
                        :unique-constraint (:unique-fields (options obs)))
    (cond
      (= time-model :sample)
      (print-create-index (apply str tname "-sample-idx") tname ["sample"])
      (= time-model :mutating)
      (print-create-index (apply str tname "-ts") tname ["ts"]))
    (if (:indexes obs)
      (doseq [idx (:indexes obs)]
        (let [iname (str (name (relation obs)) "-"
                         (apply str (interpose "-" idx)) "-idx")]
          (print-create-index iname tname idx)))
      (doseq [arg (args obs)]
        (when (:index (meta arg))
          (let [iname (str (name (relation obs)) "-" arg "-idx")]
            (print-create-index iname tname [arg])))))))

(defn print-observation-schema []
  (print-create-table "observation_types"
                      [["name" "VARCHAR PRIMARY KEY"]
                       ["doc" "VARCHAR"]
                       ["args" "VARCHAR[]"]
                       ["argtypes" "VARCHAR[]"]
                       ["options" "VARCHAR"]
                       ]
                      :if-not-exists true)
  (doseq [o (remove #(:omit-from-schema (:options %1)) (vals @threatbrain.engine/observation-map))]
    (print-observation-table o)
    (print "\n")
    (print-update-or-insert "observation_types"
                            {"name" (name (relation o))
                             "doc" (:doc (meta (ns-resolve (find-ns 'threatbrain.engine) (relation o))))
                             "args" (map as-id (args o))
                             "argtypes" (map #(or (:tag (meta %)) "String") (args o))
                             "options" (json/json-str (options o))
                             }
                            {"name" (name (relation o))})
    (print "\n\n")
    ))
