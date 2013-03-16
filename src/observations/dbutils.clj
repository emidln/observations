(ns observations.dbutils
  (:require [clojure.string :as str])
  (:use clojure.pprint))

(defmulti as-string
  "Coerces a value to as a basic string SQL representation.  It's a
  way for us to have a custom SQL string representation for all our
  data types -- aka our custom str."  class
  :default String)

(defmethod as-string clojure.lang.Keyword [v]
  (name v))

(defmethod as-string clojure.lang.Symbol [v]
  (name v))

(defmethod as-string Boolean [v]
  (if v "TRUE" "FALSE"))

(defmethod as-string String [v]
  v)

(defmethod as-string Float [v]
  (str v))

(defmethod as-string Number [v]
  (.toString v))

(defmethod as-string nil [v]
  "NULL")

(defmethod as-string clojure.lang.Sequential [v]
  (str "{" (apply str (interpose ", " v)) "}"))

(defn as-quoted-identifier
  "Returns a quoted SQL string, that means contains in double quotes,
  and double quotes inside the string are doubled.  This is a
  Postgresql 'quoted identifier'"  [v]
  (if (nil? v)
    "NULL"
    (str
     "\""
     (str/replace (as-string v) "\"" "\"\"")
     "\"")))
  
(defn as-quoted-constant [v]
  (if (nil? v)
    "NULL"
    (str "E'"
         (-> (as-string v)
             (str/replace "'" "''")
             (str/replace "\\" "\\\\"))
         "'")))

(defn as-quoted-hex [val]
  (let [s (str val)]
    (if (.startsWith s "\\x")
      s
      (as-quoted-constant (str "\\x" s)))))

(defn as-keyword [v]
  "Returns an SQL keyword."
  (-> (str/replace (as-string v) \- \_)
      (str/replace ":" "")
      (str/replace "?" "")))

(defn as-id [v]
  "Return an SQL Identifier string.  All our identifiers are have - converted to _ and are downcased."
  (as-keyword (clojure.string/lower-case v)))

(defn as-comment [v]
  (apply
   str
   (map
    #(str "-- " %1 "\n")
    (str/split-lines (as-string v)))))

(defn print-drop-table [table & {:keys [if-exists]
                                 :or {if-exists false}}]
  (print "DROP TABLE")
  (when if-exists (print " IF EXISTS"))
  (print " " (as-id table))
  (print ";\n"))

(defn print-create-table [name coldefs & {:keys [if-not-exists unique-constraint]
                                          :or {if-not-exists false}
                                          :as options}]
  (print "CREATE TABLE")
  (when if-not-exists
    (print " IF NOT EXISTS"))
  (print " " (as-id name) "(\n")
  (let [parts
        (concat
         (map (fn [cdef]
                (apply str "\t" (interpose " " (concat [(as-id (first cdef))] (rest cdef)))))
              coldefs)
         (when unique-constraint
           [(apply str "\tUNIQUE ( " (apply str (interpose ", " (map as-id unique-constraint))) " )")]))]
    (print (apply str
                  (interpose ",\n" parts))))
  (print "\n);\n"))
         

(defn print-create-index [name table cols]
  (print "CREATE INDEX " (as-id name) " ON " (as-id table) " ")
  (print "(" (apply str (interpose ", " (map as-id cols))) ");\n"))


(defn print-table-column-indexes [table & cols]
  (doseq [col cols]
    (let [cspec (map as-id (if (seq? col)
                             col
                             (list col)))
          iname (str (as-id table) "_" (apply str (interpose "_"  cspec)) "_idx")]
      
      (print-create-index iname table cspec))))


(defn insert-statement [table valmap]
  (let [insertsql (str "( "
                       (apply str (interpose "," (map as-id (keys valmap))))
                       " ) VALUES ( "
                       (apply str (interpose "," (map as-quoted-constant (vals valmap))))
                       " )")]
    (str "INSERT INTO " (as-id table) " " insertsql ";")
    ))

(defn print-insert [table valmap]
  (println (insert-statement table valmap))
)


(defn multi-row-insert-statement [table keys vlists]
  (str "INSERT INTO " (as-id table) " "
       (str "( " (apply str (interpose "," (map as-id keys))) " ) VALUES\n"
            (apply str
                   (interpose ",\n"
                              (map
                               (fn [vals]
                                 (str "\t(" (apply str (interpose "," (map as-quoted-constant vals))) ")"))
                               vlists))))
       ";")
  )

(defn print-multi-row-insert [table keys vlists]
  (println (multi-row-insert-statement table keys vlists))
  )
