(ns birdseye.tmp-notes
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


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

#_(defn node-to-url2 [site-map key params-map]
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

#_(defn interactive-test []

  ;; (pp/pprint
  ;;  (gen-url-matcher site-map))
  (pp/pprint
   (let [matcher (gen-matcher site-map)]
     (matcher "/locations/")))

  ;; (pp/pprint
  ;;  (let [params {:loc-id 1234
  ;;                :userid 987
  ;;                :lang-id "EN"
  ;;                :var "foo"}]
  ;;    (for [k (keys site-map)]
  ;;      [(node-to-url2 site-map k params)
  ;;       (node-key-to-routing-key k)])))
  )

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


  (deftype SiteMap [sitemap]
    ApplicationMapper
                                        ; ... implement the stuff above
    )

  (deftype SiteNode [sitemap node-key]
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
