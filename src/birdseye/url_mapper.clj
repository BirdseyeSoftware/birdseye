(ns birdseye.url-mapper
  (:require [clojure.string :as string])
  (:import java.net.URLDecoder))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

;; (defprotocol UrlMapper
;;   (url-to-node [this url]) ; -> [node-key params-map]
;;   (node-to-url [this params-map]))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-decode
  "Decode a path segment in a URI. Defaults to using UTF-8 encoding."
  ([path]
     (path-decode path "UTF-8"))
  ([path encoding]
     (string/replace
      path
      #"(?:%[0-9A-Fa-f]{2})+"
      #(URLDecoder/decode % encoding))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def node-key-segment-re #"\.")
(defn split-node-key [k]
  (string/split (name k) node-key-segment-re))

(defn- dynamic? [key-segment]
  (= (first key-segment) \$))

(defn- decompose-dyn-segment [key-segment]
  (let [prefix (first key-segment)
        id (apply str (rest key-segment))]
    [prefix id]))

(defn- dyn-segment-id [key-segment]
  (keyword (second (decompose-dyn-segment key-segment))))

(defn node-to-url2 [site-map key params-map]
  (if (contains? #{:home :root} (keyword key))
    "/"
    (let [normalized-segs
          (map (fn [seg]
                 (if (dynamic? seg)
                   (let [seg-key (dyn-segment-id seg)]
                     (if (contains? params-map seg-key)
                       (params-map seg-key)
                       (throwf "missing required url parameter: %s" seg-key)))
                   seg))
               (split-node-key key))]
      (str "/" (string/join "/" normalized-segs) "/"))))

(defn node-to-route-map-entry [k context]
  (let [[static dynamic] (split-with
                          #(not (dynamic? %))
                          (split-node-key k))]
    (if (seq dynamic)
      [static dynamic]
      [static k])))

(defn split-url-path [url]
  (string/split (path-decode url) "/"))

(def dynamic ::dynamic)

(defn node-key-to-routing-key [k]
  (let [segments (split-node-key k)]
    (map (fn [seg] (if (dynamic? seg) dynamic
                       seg)) segments)))

(defn split-url [u]
  (string/split u #"/"))

(defn url-path-to-routing-key [u is-known? ]
  (let [segments (split-url u)
        segments (if (= (first segments) "") (rest segments) segments)]
    (map (fn [seg] (if (is-known? seg) dynamic seg)) segments)))

(defn gen-routing-table [site-map]
  ;@@TR: add static mapping with URL strings
  (into {} (map
            (fn [[k v]]
              [(node-key-to-routing-key k) k])
       site-map)))

(defn gen-token-set [routing-table]
  (set (filter #(not (= dynamic %)) (flatten (keys routing-table)))))

(defn gen-matcher [site-map & expression-table]
  (let [routing-table (gen-routing-table site-map)
        token-set (gen-token-set routing-table)]
    (fn matcher [url-path]
      (let [url-routing-key (url-path-to-routing-key url-path token-set)]
        (routing-table url-routing-key)))))

(comment
  ;; just some examples to play with
  (def expressions
    ;; global table of path
    ;; expressions to avoid repetition in the map itself
    {:loc-id #"[0-9]+"
     :var #"v.*"})

  (def site-map
    '(
      home

      locations
      locations.$loc-id {:expressions {:loc-id #"[0-9]+"}}
      locations.$loc-id.languages {:expressions {:loc-id #"[0-9]+"}}

      users
      users.$user-id user-nodes)))
