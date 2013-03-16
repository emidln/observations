(defproject observations "0.1.0-SNAPSHOT"
  :description "Observations are core.logic defrels that can be mapped to an SQL schema and stored there"  
  :url "https://github.com/threatgrid/observations"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.logic "0.7.5"]
                 [org.clojure/tools.logging "0.2.3"]
                 [hiccup "1.0.2"]
                 [pldb "0.1.1"]])
