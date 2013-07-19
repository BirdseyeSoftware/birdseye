(ns birdseye.macros
  (:require
   [clojure.core.match :refer [match]]
   [birdseye.sitemap :refer [sitemap?
                             split-node-key
                             relative-node-key?
                             join-node-key-segments
                             node-key-segment-separator]]))

(defn unquoted?
  "Check if the form is syntax-unquoted: like ~form"
  [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sitemap definition related code

(defn- node-children? [o]
  (or (vector? o) (sitemap? o)))

(declare -gen-sitemap)
(defn -handle-children [node-key context-map children]
  (let [sexp-count (if (nil? context-map) 2 3)
        children (if (not (sitemap? children))
                   (-gen-sitemap children)
                   children)
        context-map (or context-map {})
        match (if (contains? children :.index)
                {:node-key node-key
                 :context-map (merge (:.index children) context-map)
                 :children (dissoc children :.index)
                 :sexp-count sexp-count}
                {:node-key node-key
                 :context-map context-map
                 :children children
                 :sexp-count sexp-count})]
    match))

(defn -match-sitemap-forms [forms]
  (let [nil-value? (fn [v]
                     (or (nil? v)
                         (#{:nil 'nil} v)))
        error (fn [val msg & [sexp-count]]
                {:error val :message msg :sexp-count (or sexp-count 1)})
        next-entry (match [forms]
                     [([(k :guard nil-value?) & r] :seq)]
                     (error :nil "nil is not a valid node-key")

                     [([(k :guard false?) & r] :seq)]
                     (error :false "false is not a valid node-key")

                     [([(k :guard keyword?) (next-k :guard keyword?) & r] :seq)]
                     {:node-key k}

                     [([(k :guard keyword?)
                        (context-map :guard map?)
                        (children :guard node-children?) & r] :seq)]
                     (-handle-children k context-map children)

                     [([(k :guard keyword?) (children :guard node-children?) & r] :seq)]
                     (-handle-children k nil children)

                     [([(k :guard keyword?) (context-map :guard map?) & r] :seq)]
                     {:node-key k :context-map context-map}

                     [([(k :guard keyword?) & r] :seq)]
                     {:node-key k}

                     [([(sym :guard symbol?) & r] :seq)]
                     (error {:invalid-entry sym}
                            (str
                             "Invalid sitemap entry. Node keys "
                             "should be :keywords not symbols "
                             "by the time this code is reached."))

                     [_]
                     (error {:invalid-entry (first forms)}
                            "Invalid sitemap entry"))]
    (update-in next-entry [:sexp-count]
               #(or % (count next-entry)))))

(defn -normalize-map-forms [mapforms & [prefix]]
  (let [prefix (and prefix (name prefix))
        normalize-key (fn [k-name]
                        (if (and prefix (= \. (first k-name)))
                          (str prefix k-name)
                          k-name))

        named? (fn named? [x] (instance? clojure.lang.Named x))]
    (into []
          (for [form mapforms]
            (cond
              ;; match any symbols that should be inserted by value rather
              ;; by than name
              (unquoted? form)
              (second form)

              (and (named? form)
                   (not (re-find #"/" (name form))))
              (keyword (normalize-key (name form)))

              (vector? form) (apply vector (-normalize-map-forms form prefix))

              :else
              form)))))

(defn- -normalize-node-children [children parent-key]
  (assert (node-children? children))
  (-normalize-map-forms
   (if (sitemap? children)
     (flatten (seq children))
     children)
   parent-key))

(defn- -throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- -assert-parent-node-exists [node-key sitemap]
  (let [segments (split-node-key node-key)
        n-segs (count segments)
        parent-key (if (and (> n-segs 1)
                            (not (and (= n-segs 2)
                                      (relative-node-key? node-key))))
                     (join-node-key-segments
                      (take (- n-segs 1) segments)))]
    (if (and parent-key
             (not (contains? sitemap parent-key)))
      (-throwf
       "Invalid site-node key '%s'.
       Parent node %s does not exist. %s"
       node-key parent-key (keys sitemap)))))

(defn- -validate-sitemap-addition [sitemap index-in-forms node-key context-map]
  (if (not (keyword? node-key))
    (-throwf (str "Was expecting a sitemap node-key"
                  " in defsitemap position %s") index-in-forms))
  (if (= node-key-segment-separator (last (name node-key)))
    (-throwf "Node keys must not end in a dot: %s" node-key))
  (if (sitemap node-key)
    (-throwf "%s is already in the sitemap %s." node-key sitemap))
  (-assert-parent-node-exists node-key sitemap)
  (if (not (or (map? context-map)
               (nil? context-map)))
    (-throwf "Was expecting a sitemap node context map, i.e. a hash-map
          not a %s." (type context-map))))

(defn -gen-sitemap [mapforms & [sitemap0]]
  (let [mapforms (-normalize-map-forms mapforms)
        n (count mapforms)]
    (loop [i 0
           sitemap (if sitemap0 sitemap0 (sorted-map))]
      (if (< i n)
        (let [rest-forms (drop i mapforms)
              match (-match-sitemap-forms rest-forms)
              {:keys [node-key children context-map error sexp-count]
               :or {context-map {}}} match
              j (+ i sexp-count)]
          (if error
            (-throwf
             "Invalid sitemap entry at position %s: %s" i (:error match)))
          (-validate-sitemap-addition sitemap i node-key context-map)
          (let [sitemap' (assoc sitemap node-key context-map)
                sitemap' (if children
                           (-gen-sitemap (-normalize-node-children
                                          children node-key)
                                         sitemap')
                           sitemap')]
            (recur j sitemap')))
        (with-meta sitemap
          ;; Could alternatively make the map a record
          ;; which would avoid the possibility of this key being lost.
          ;; However, the map wouldn't be a sorted-map then.
          {:birdseye/sitemap true})))))

(defmacro gen-sitemap [mapforms]
  `(-gen-sitemap ~(-normalize-map-forms mapforms)))

(defmacro defsitemap [name mapforms-vec & body]
  `(do
     (def ~name
       (-> (gen-sitemap ~mapforms-vec)
           ~@body))))
