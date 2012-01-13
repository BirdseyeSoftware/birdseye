(ns birdseye.core
  (:refer-clojure :exclude [sym])
  (:require [clojure.string :as string])

  (:require [clojure.core.match :as match])
  (:require [clout.core :as clout])

  (:import java.net.URLDecoder))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sitemap node-key related funcs and constants
(defonce node-key-segment-separator \.)
(defonce node-key-segment-re #"\.")
(defonce node-key-dyn-segment-prefix \$)
(defonce node-key-dyn-re #"\$")

(defn relative-node-key? [k]
  (= node-key-segment-separator (first (name k))))

(defn- dynamic-node-key? [k]
  (boolean (re-find node-key-dyn-re (name k))))

(defn- dynamic-node-key-seg? [key-segment]
  (= (first key-segment) node-key-dyn-segment-prefix))

(defn split-node-key [k]
  (string/split (name k) node-key-segment-re))

(defn join-node-key-segments [segments]
  (keyword (string/join node-key-segment-separator segments)))

(defn- decompose-dyn-segment [key-segment]
  (let [prefix (first key-segment)
        id (apply str (rest key-segment))]
    [prefix id]))

(defn- dyn-segment-id [key-segment]
  (if (dynamic-node-key-seg? key-segment)
    (keyword (second (decompose-dyn-segment key-segment)))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sitemap definition related code
(defn sitemap? [o]
  (and (map? o) (::sitemap (meta o))))

(defn relative-sitemap? [sm]
  (every? relative-node-key? (keys sm)))

(defn absolute-sitemap? [sm]
  (not-any? relative-node-key? (keys sm)))

(defn- node-children? [o]
  (or (vector? o) (sitemap? o)))

(defn match-sitemap-forms [forms]
  (let [any-form-of-nil? (fn [v]
                           (or (nil? v) (#{:nil 'nil} v)))]
    (match/match [forms]
      [([(nil-as-k :when any-form-of-nil?) & r] :seq)]
      {:error :nil :message "nil is not a valid node-key"}

      [([(k :when false?) & r] :seq)]
      {:error :false :message "false is not a valid node-key"}

      [([(k :when keyword?) (next-k :when keyword?) & r] :seq)]
      {:node-key k}

      [([(k :when keyword?) (children :when node-children?) & r] :seq)]
      {:node-key k :children children}

      [([(k :when keyword?)
         (context-map :when map?)
         (children :when node-children?) & r] :seq)]
      {:node-key k :context-map context-map :children children}

      [([(k :when keyword?) (context-map :when map?) & r] :seq)]
      {:node-key k :context-map context-map}

      [([(k :when keyword?) & r] :seq)]
      {:node-key k}

      [_]
      {:error (first forms)})))

(defn- normalize-map-forms [mapforms & prefix]
  (let [prefix (and prefix (name (first prefix)))
        normalize-key (fn [k-name]
                        (if (and prefix
                                 (= \. (first k-name)))
                          (str prefix k-name)
                          k-name))
        named? (fn  [x] (instance? clojure.lang.Named x))]
    (for [form mapforms]
      (cond
        ;; match any symbols that should be inserted by value rather
        ;; by than name
        (and (named? form)
             (re-find #"=" (name form)))
        (symbol (apply str (rest (name form))))

        (and (named? form)
             (not (re-find #"/" (name form))))
        (keyword (normalize-key (name form)))

        (vector? form) (apply vector (normalize-map-forms form))

        :else
        form))))

(defn- normalize-node-children [children parent-key]
  (assert (node-children? children))
  (normalize-map-forms
   (if (sitemap? children)
     (flatten (seq children))
     children) parent-key))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- assert-parent-node-exists [node-key sitemap]
  (let [segments (split-node-key node-key)
        n-segs (count segments)
        parent-key (if (and (> n-segs 1)
                            (not (and (= n-segs 2)
                                      (relative-node-key? node-key))))
                     (join-node-key-segments
                      (take (- n-segs 1) segments)))]
    (if (and parent-key
             (not (sitemap parent-key)))
      (throwf
       "Invalid site-node key '%s'.
       Parent node '%s' does not exist. %s"
       node-key parent-key (keys sitemap)))))

(defn- validate-sitemap-addition [sitemap index-in-forms node-key context-map]
  (if (not (keyword? node-key))
    (throwf (str "Was expecting a sitemap node-key"
                 " in defsitemap position %s") index-in-forms))
  (if (= node-key-segment-separator (last (name node-key)))
    (throwf "Node keys must not end in a dot: %s" node-key))
  (if (sitemap node-key)
    (throwf "%s is already in the sitemap." node-key))
  (assert-parent-node-exists node-key sitemap)
  (if (not (or (map? context-map)
               (nil? context-map)))
    (throwf "Was expecting a sitemap node context map, i.e. a hash-map
          not a %s." (type context-map))))

(defn gen-sitemap [mapforms & sitemap0]
  (let [n (count mapforms)]
    (loop [i 0
           sitemap (if sitemap0
                     (first sitemap0) (sorted-map))]
      (if (< i n)
        (let [rest-forms (drop i mapforms)
              match (match-sitemap-forms rest-forms)
              {:keys [node-key children context-map error]
               :or {context-map {}}} match
              j (+ i (count match))]
          (if error
            (throwf
             "Invalid sitemap entry at position %s: %s" i (:error match)))
          (validate-sitemap-addition sitemap i node-key context-map)
          (let [sitemap' (assoc sitemap node-key context-map)
                sitemap' (if children
                           (gen-sitemap (normalize-node-children
                                         children node-key)
                                        sitemap')
                           sitemap')]
            (recur j sitemap')))
        (with-meta sitemap
          ;; could alternatively make the map a record
          ;; which would avoid the possibility of this key being lost
          {::sitemap true})))))

(defmacro defsitemap [& mapforms]
  `(gen-sitemap (vector ~@(normalize-map-forms mapforms))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; url-mapping

(defprotocol IUrlMapper
  (url-to-node [this url]) ; -> [node-key params-map]
  (node-to-url [this node-key params-map]))

(deftype RingApp [sitemap url-generator url-matcher]
  IUrlMapper
  (node-to-url [this node-key params-map]
    (url-generator node-key params-map) )

  (url-to-node [this url] (url-matcher url)))

(defn gen-static-url [node-key]
  (if (not (dynamic-node-key? node-key))
    (if(contains? #{:home :root} (keyword node-key))
      "/"
      (str "/" (string/join "/" (split-node-key node-key)) "/"))))

(defn gen-dynamic-url-matcher [node-key regexes]
  (let [clout-pattern-segs
        (map (fn [seg]
               (if (dynamic-node-key-seg? seg)
                 (string/replace seg node-key-dyn-re ":")
                 seg))
             (split-node-key node-key))
        clout-pattern (str "/" (string/join "/" clout-pattern-segs) "/")
        clout-route (clout/route-compile
                     clout-pattern regexes)]
    (fn [url-path]
      (clout/route-matches clout-route {:path-info url-path}))))

(defn gen-url-matcher [sitemap & regexes]
  (let [regexes (if (map? (first regexes))
                  (first regexes)
                  (apply hash-map regexes))
        {static-keys false,
         dynamic-keys true} (group-by dynamic-node-key? (keys sitemap))
        static-map (into {} (for [k static-keys] [(gen-static-url k) k]))
        dynamic-routes (map
                        (fn [k] [(gen-dynamic-url-matcher k regexes) k])
                        dynamic-keys)
        match-static #(if-let [k (static-map %)] [k {}])
        match-dyn (fn [url]
                    ;; TODO
                    ;; this could be optimized with static lookup of
                    ;; any leading static segments
                    (some (fn [[matcher nk]]
                            (if-let [groups (matcher url)]
                              [nk groups]))
                          dynamic-routes))]
    (fn url-to-node [url]
      (or (match-static url)
          (match-dyn url)
          [nil {}]))))

(defn- gen-dynamic-url [node-key params-map]
  ;; TODO
  ;; need to check on param url-escaping here
  (format
   "/%s/"
   (string/join
    "/"
    (for [seg (split-node-key node-key)]
      (if-let [param-id (dyn-segment-id seg)]
        (if (contains? params-map param-id)
          (params-map param-id)
          (throwf "missing required url parameter: %s" param-id))
        seg)))))

(defn url-generator [node-key params-map]
  (or (gen-static-url node-key)
      (gen-dynamic-url node-key params-map)))

(defn gen-ring-app [sitemap]
  (RingApp. sitemap url-generator (gen-url-matcher sitemap)))
