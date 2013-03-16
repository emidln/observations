(ns observations.dochelper
  (:use hiccup.core))


(defn doc-observation
  "Generate hiccup data structure documenting the given observation type."
  [obs]
  [:div.row.observation-type {:id (str "relation-" (name (:relation obs)))}
   [:div.span12
    [:h3 (name (:relation obs))]
    [:p (:doc (:options obs))]
    [:h4 "Arguments"]
    [:table.table.args.table-condensed.table-striped
     [:thead
      [:tr
       [:th "Name"]
       [:th "Type"]
       [:th "Required?"]
       [:th "Indexed?"]
       [:th "Description"]
       ]]
     
     (map (fn [arg]
            (let [attr (meta arg)]
              [:tr.arg
               [:th.arg-name
                [:a {:href (str "#arg-" arg)} arg]]
               [:td.arg-tag
                [:a {:href (str "#type-" (:tag attr))}
                 (:tag attr)]]
               [:td.arg-required (:required attr)]
               [:td.art-indexed (:index attr)]
               [:td.arg-doc (:doc attr)]
               ]))
          (:args obs))]]
   [:div.span6
    [:h4 "Options"]
    [:table.table.options.table-condensed
     [:tr
      [:th "Origin: " [:td (:origin (:options obs))]]
      [:th "Exported: " [:td (:export? (:options obs))]]]
     [:tr
      [:th "Time Model: " [:td (:time-model (:options obs))]]
      [:th "Tags: " [:td
                     (map (fn [t]
                            (list [:a {:href (str "#tag-" t)}
                                   t]
                                  "&nbsp;&nbsp;"))
                          (:tags (:options obs)))]]]]]
   ]
  )


(defn observation-type-tag-map
  "Returns a map with observation type tags as keys, and the corresponding observation types as values."
  [obs]
  (let [tags
        (sort-by clojure.string/lower-case
                 (set
                  (mapcat (fn [o] (:tags (:options o))) obs)))]
    (zipmap
     tags
     (map (fn [tag]
            (filter
             #(some #{tag} (:tags (:options %)))
             obs))
          tags))))

(defn observation-type-arg-map
  "Returns a map with observation type argument names as keys, and the
corresponding observation types as values."
  [obs]
  (let [tags
        (sort-by clojure.string/lower-case
                 (set
                  (mapcat (fn [o] (:args o)) obs)))]
    (zipmap
     tags
     (map (fn [tag]
            (filter
             #(some #{tag} (:args %))
             obs))
          tags))))

(defn observation-argument-type-map
  "Returns a map with observation type argument types (the :tag
metadata value) as keys, and the corresponding observation types as
values."
  [obs]
  (let [tags
        (sort
         (set
          (mapcat (fn [o] (map #(:tag (meta %))
                               (:args o)))
                  obs)))]
    (zipmap
     tags
     (map (fn [tag]
            (filter
             #(some #{tag}
                    (map (fn [a]
                           (:tag (meta a)))
                         (:args %)))
             obs))
          tags))))

(defn generate-observation-docs
  "Returns an HTML document describing all observation types,
including an overview and several indexes."
  [obs]
  (html
   [:head
    [:script {:type "text/javascript"}
     (slurp (clojure.java.io/resource "assets/javascripts/jquery.min.js"))
     (slurp (clojure.java.io/resource "assets/javascripts/bootstrap.js"))
     (slurp (clojure.java.io/resource "assets/javascripts/report.js"))]
    [:style {:type "text/css"}
     ".observation-type-index a { font-size: small;}\n"
     (slurp (clojure.java.io/resource "assets/css/bootstrap.min.css"))
     (slurp (clojure.java.io/resource "assets/css/bootstrap-responsive.min.css"))
     (slurp (clojure.java.io/resource "assets/css/report.css"))]]
   [:body
    [:div.container
     [:div.row
      [:div.span12
       [:header
        [:div.subnav
         [:ul.nav.nav-pills
          [:li [:a {:href "#overview"}
                "Overview"]]
          [:li [:a {:href "#observation-types"}
                "Observation Types"]]
          [:li [:a {:href "#tag-index"}
                "Tag Index"]]
          [:li [:a {:href "#arg-index"}
                "Arg Index"]]
          [:li [:a {:href "#type-index"}
                "Type Index"]]]]]]]
     [:h1 {:id "overview"} "Overview"]
     [:div.row.overview
      [:div.span12
       ]]
     [:h2 {:id "observation-types"} "Observation Types"]
     [:div.observation-type-index
      (map (fn [o]
             (list [:a {:href (str "#relation-" (name o))}
                    (name o)]
                   "&nbsp;|&nbsp;"))
           (sort (map :relations obs)))]
     (map (fn [obs]
            [:div.row
             [:div.span12
              (doc-observation obs)
              [:hr]]])
          (sort-by :relation obs))
     [:h2 {:id "tag-index"} "Tag Index"]
     [:div.row
      [:div.span12
       [:table.table.table-condensed
        [:thead
         [:tr
          [:th "Tag"]
          [:th "Observation Types"]]]
        (map
         (fn [tm]
           [:tr
            [:td {:id (str "tag-" (first tm))}
             (first tm)]
            [:td.observation-type-index
             (map (fn [o]
                    (list [:a {:href (str "#relation-" (name (:relation o)))}
                           (name (:relation o))]
                          "&nbsp;&nbsp;"))
                  (second tm))]])
         (observation-type-tag-map))]]]

     [:h2 {:id "arg-index"} "Argument Index"]
     [:div.row
      [:div.span12
       [:table.table.table-condensed
        [:thead
         [:tr
          [:th "Argument name"]
          [:th "Observation Types"]]]
        (map
         (fn [tm]
           [:tr
            [:td {:id (str "arg-" (first tm))}
             (first tm)]
            [:td.observation-type-index
             (map (fn [o]
                    (list [:a {:href (str "#relation-" (name (:relation o)))}
                           (name (:relation o))]
                          "&nbsp;&nbsp;"))
                  (second tm))]])
         (observation-type-arg-map))]]]
     
     [:h2 {:id "type-index"} "Type Index"]
     [:div.row
      [:div.span12
       [:table.table.table-condensed
        [:thead
         [:tr
          [:th "Type"]
          [:th "Observation Types"]]]
        (map
         (fn [tm]
           [:tr
            [:td {:id (str "type-" (first tm))}
             (first tm)]
            [:td.observation-type-index
             (map (fn [o]
                    (list [:a {:href (str "#relation-" (name (:relation o)))}
                           (name (:relation o))]
                          "&nbsp;&nbsp;"))
                  (second tm))]])
         (observation-argument-type-map))]]]
     ]
    ]
   ))
  