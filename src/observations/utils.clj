(ns observations.utils)

(defn ensure-namespace
  "If sym contains a namespace prefix for a loaded namespace or namespace
alias, returns the symbol with the full namespace name, as prefix (so, no change if
already has a full namespace prefix). Could an alias shadow a namespace, though?
Maybe we should reverse the order of that or clause."
  [sym] 
  (when (symbol? sym)
    (symbol (name
             (ns-name
              (or (when (namespace sym)
                    (or (find-ns (symbol (namespace sym)))
                        (get (ns-aliases *ns*) (symbol (namespace sym)))))
                  *ns*)))
            (name sym))))

(defn compact [seq]
  (filter #(and (identity %1)
                (not (empty? %1)))
          seq))
