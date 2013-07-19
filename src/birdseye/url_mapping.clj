(ns birdseye.url-mapping
  (:require [clojure.string :as string])
  (:require [clout.core :as clout])
  (:require [birdseye.sitemap :refer
             [split-node-key
              dyn-segment-id
              dynamic-node-key?
              dynamic-node-key-seg?
              node-key-dyn-re]]))

(defprotocol IUrlMapper
  (url-to-node [this url-path]) ; -> [node-key params-map]
  (node-to-url [this node-key params-map]))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url generation from node-keys

(defn -gen-static-url [node-key]
  (if (not (dynamic-node-key? node-key))
    (if(contains? #{:home :root} (keyword node-key))
      "/"
      (str "/" (string/join "/" (split-node-key node-key)) "/"))))

(defn -gen-dynamic-url [node-key params-map]
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
  (or (-gen-static-url node-key)
      (-gen-dynamic-url node-key params-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url-matching

(defn -gen-dynamic-url-matcher [node-key regexes]
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
        static-map (into {} (for [k static-keys] [(-gen-static-url k) k]))
        dynamic-matchers (map
                          (fn [k]
                            [(-gen-dynamic-url-matcher
                              k (merge regexes
                                       (get-in sitemap [k :regexes])))
                             k])
                          dynamic-keys)
        match-static #(if-let [k (static-map %)] [k {}])
        match-dyn (fn [url]
                    ;; TODO
                    ;; this could be optimized with static lookup of
                    ;; any leading static segments
                    (some (fn [[matcher nk]]
                            (if-let [groups (matcher url)]
                              [nk groups]))
                          dynamic-matchers))]
    ;; TODO implement 404 lookup mechanism that walks up the tree
    ;; from the closest node match and looks for a 404 handler there
    (fn url-to-node [url]
      (or (match-static url)
          (match-dyn url)
          [:http-404
           {} ; this second val is the context map for the :404 node
           ]))))

(deftype UrlMapper [url-matcher url-generator]
  IUrlMapper
  (node-to-url [this node-key params-map]
    (url-generator node-key params-map))
  (url-to-node [this url-path] (url-matcher url-path)))

(defn gen-default-url-mapper [sitemap]
  (UrlMapper. (gen-url-matcher sitemap) url-generator))
