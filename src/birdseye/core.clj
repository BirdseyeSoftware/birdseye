(ns birdseye.core

  (:require [clojure.string :as string]))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn named? [x]
  (instance? clojure.lang.Named x))

(def node-key-segment-separator ".")
(def node-key-segment-re #"\.")

(defn split-node-key [k]
  (string/split (name k) node-key-segment-re))

(defn join-node-key-segments [segments]
  (keyword (string/join node-key-segment-separator segments)))

(defn- dynamic? [key-segment]
  (= (first key-segment) \$))

(defn- decompose-dyn-segment [key-segment]
  (let [prefix (first key-segment)
        id (apply str (rest key-segment))]
    [prefix id]))

(defn- dyn-segment-id [key-segment]
  (keyword (second (decompose-dyn-segment key-segment))))

(defn- assert-parent-node-exists [node-key site-map]
  (let [segments (split-node-key node-key)
        n-segs (count segments)
        parent-key (if (> n-segs 1)
                     (join-node-key-segments
                      (take (- n-segs 1) segments)))]
    (if (and parent-key
             (not (site-map parent-key)))
      (throwf
       "Invalid site-node key '%s'.
 Parent node '%s' does not exist."
       node-key parent-key))))

(defn- validate-site-map-addition [site-map index-in-forms node-key context-map]
  (if (not (keyword? node-key))
    (throwf "Was expecting a site-map node-key in defsitemap position %s" index-in-forms))
  (if (site-map node-key)
    (throwf "%s is already in the site-map." node-key))
  (assert-parent-node-exists node-key site-map)
  (if (not (or (map? context-map)
               (nil? context-map)))
    (throwf "Was expecting a site-map node context map, i.e. a hash-map
          not a %s." (type context-map))))

(defn gen-sitemap [& mapforms]
  (let [n (count mapforms)]
    (loop [i 0
           site-map {}]
      (if (< i n)
        (let [node-key (nth mapforms i)
              next-form (if (< (+ 1 i) n)
                          (nth mapforms (+ 1 i)))
              context-map-next? (not (keyword? next-form))
              context-map (or (if context-map-next? next-form) {})
              next-i (if context-map-next? (+ 2 i) (+ 1 i))]
          (validate-site-map-addition site-map i node-key context-map)
          (recur next-i (assoc site-map node-key context-map)))
        site-map))))

(defn normalize-map-forms [mapforms]
  ;@@TR: handle the insertion of relative sub-node sequences
  (for [form mapforms]
    (cond
      (named? form)
      (keyword (name form))
      :else
      form)))

(defmacro defsitemap [& mapforms]
  `(gen-sitemap ~@(normalize-map-forms mapforms)))


(comment


  (defprotocol ApplicationMapper
    ;; the router / middleware
    (get-node-key-from-ring-req [this req])
    (get-node-ctx-from-ring-req [this req])
    (get-node-ctx [this node-key]))

  (defprotocol NodeContext
    ;; the user of these is the outermost app/controller layer
    (get-controller [this]) ; could use compojure routing or other
    (get-view [this req resp]) ; any function that transforms a resp

    ;; the user of these is the 'view' or maybe the controller
    (get-url [this & params])
    (get-crumb [this])
  ;;; get-params required/optional
    )


  (deftype SiteMap [site-map]
    ApplicationMapper
                                        ; ... implement the stuff above
    )

  (deftype SiteNode [site-map node-key]
    NodeContext
                                        ;...
    )

  (defprotocol RingAppMapper
    (req-to-node-key [this req])
    (req-to-node-ctx [this req])
    (augment-ring-request [this req]))

  (defn handle-request
    "The outermost request handler below standard ring middleware"
    [req]
    (let [node-ctx          (req-to-node-ctx req)
          req               (augment-ring-request node-ctx req)
          controller        (get-controller node-ctx)
          initial-resp      (controller req)
          view              (get-view node-ctx req initial-resp)
          final-resp        (view req initial-resp)]
      final-resp))
  )
