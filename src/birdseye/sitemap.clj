(ns birdseye.sitemap
  (:require [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sitemap node-key related funcs and constants

(defonce node-key-segment-separator \.)
(defonce node-key-segment-re #"\.")
(defonce node-key-dyn-segment-prefix \$)
(defonce node-key-dyn-re #"\$")

(defn relative-node-key? [k]
  (= node-key-segment-separator (first (name k))))

(defn dynamic-node-key? [k]
  (boolean (re-find node-key-dyn-re (name k))))

(defn dynamic-node-key-seg? [key-segment]
  (= (first key-segment) node-key-dyn-segment-prefix))

(defn split-node-key [k]
  (string/split (name k) node-key-segment-re))

(defn join-node-key-segments [segments]
  (keyword (string/join node-key-segment-separator segments)))

(defn decompose-dyn-segment [key-segment]
  (let [prefix (first key-segment)
        id (apply str (rest key-segment))]
    [prefix id]))

(defn dyn-segment-id [key-segment]
  (if (dynamic-node-key-seg? key-segment)
    (keyword (second (decompose-dyn-segment key-segment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates

(defn sitemap? [o]
  (boolean (and (map? o) (:birdseye/sitemap (meta o)))))

(defn relative-sitemap? [sm]
  (every? relative-node-key? (keys sm)))

(defn absolute-sitemap? [sm]
  (not-any? relative-node-key? (keys sm)))

(defn valid-sitemap? [sm]
  (and (sitemap? sm)
       (every? keyword? (keys sm))
       (every? map? (vals sm))))
