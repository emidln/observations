(ns observations.core
  (:use observations.utils)
  (:use [clojure.tools.logging :only (info error debug warn)])
  (:require [pldb.logic :as dblogic]))

;; observations are simple factual statements about a run that we get
;; from the sandcastle files.  WE predefine the types that can be
;; seen, as they are the basis for building rules, and we want to have
;; a dictionary of them available to analysts
(def observation-map (ref {}))

(defprotocol IObservation
  (relation [this])
  (args [this])
  (options [this])
  (csv-source [this])
  (json-source [this])
  (fn-source [this])
  (prepare-fn [this]))

(defrecord Observation
    [relation
     args
     options
     csv-source
     json-source
     fn-source 
     prepare-fn]

  IObservation
  (relation [_] relation)
  (args [_] args)
  (options [_] options)
  (csv-source [_] csv-source)
  (json-source [_] json-source)
  (fn-source [_] fn-source)
  (prepare-fn [_] prepare-fn))


(defn- docstring-for-element [el]
  (format "%s\tType: %s,  Required: %s, Indexed: %s\n%s\n"
          (name el)
          (if-let [t (:tag (meta el))] (name t) "*")
          (if (:required (meta el)) "yes" "no")
          (if (:index (meta el)) "yes" "no")
          (if-let [d (:doc (meta el))]
            (str "\t" d "\n")
            "")))
  
(defn- generate-observation-docstring [rel elements options]
  (str (:doc options)
       "\n\n"
       "Form: (" rel " "
       (apply str (interpose " " (map (fn [a] (name a)) elements))) ")\n\n"
       "Elements:\n"
       (apply str (map (fn [a] (docstring-for-element a)) elements))))


(defn observation-rows [oname]
  (let [oname (ensure-namespace oname)
        arity (count (:args (get @observation-map oname)))
        setvar-name (symbol (str oname "_" arity "-set"))
        setvar (try
                 (resolve (find-ns (symbol (namespace oname))) setvar-name)
                 (catch Exception e#
                   (error "Unable to resolve set variable: " setvar-name)
                   (throw e#)))
        aset (try
               (var-get setvar)
               (catch Exception e#
                 (error "Could not lookup the var: " setvar " with name: " setvar-name)
                 nil))]
    (when aset (deref aset))))


(defmacro defobs [rel elements & {:keys [doc csv-source json-source
                                         fn-source prepare-fn
                                         unique-fields scope
                                         persistant? tags export?
                                         origin]
                                  :or {doc "Undocumented observation type"
                                       export? true
                                       origin :analysis
                                       scope []
                                       }
                                  :as options}]
  (let [docstr (generate-observation-docstring rel elements options)]
    ;; add it to the IOC map
    `(let [a# (Observation.
               (quote ~(ensure-namespace rel))
               (quote ~elements)
               (quote ~(merge options {:export? export? :origin origin :scope scope}))
               ~csv-source
               ~json-source
               ~fn-source
               ~prepare-fn)]
       (dosync (alter observation-map
                      #(assoc %1 (quote ~(ensure-namespace rel)) a#)))
       
       (dblogic/db-rel ~(with-meta rel {:doc docstr})
                       ~@elements))))

(defn observe [rel & args]
  (apply vector rel args))

(defn extend-db-with-fact [db rel & fact]
  (apply dblogic/db-fact db rel fact))

(defmulti generate-observations
  "Generates observations from the existing analysis context.  Only
  argument is the name of the observations to generate.  This runs
  after all analysis based observations have been loaded."
  identity)

(defmethod generate-observations :default [_]
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn group-by-observation [oblist]
  (map #(list (first (first %1))
              (map rest %1))
       (partition-by first (sort-by first oblist))))

(defn load-observations [obs]
  "Loads a list of observations into the engine.  The list is vectors of the form [obtype args...]"

  (let [db-fact-reducer
        (fn [db fact]
          (try
            (apply extend-db-with-fact db fact)
            (catch RuntimeException e#
              (warn "Error while loading observation " fact " : " e#)
              db)))

        empty-db {}

        db (reduce db-fact-reducer empty-db obs)

        observation-generators
        (filter #(= :generator (:origin (:options %1))) (vals @observation-map))

        run-generator
        (fn [relation]
          (try
            (debug "Running observation generator for " relation)
            (let [result (generate-observations relation)]
              result)
            (catch Exception e#
              (warn "Error while running generate-observations for " relation
                    " Error: " e#))))
        
        generated-facts
        (dblogic/with-db db
          (debug "Running observation generators now.")
          (apply concat
                 (compact (doall
                           (for [generator observation-generators]
                             (run-generator (:relation generator)))))))
        
        final-db
        (reduce db-fact-reducer db generated-facts)]
    final-db))
