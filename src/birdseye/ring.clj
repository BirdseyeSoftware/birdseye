(ns birdseye.ring
  (:require [clojure.string :as string])
  (:import [clojure.lang IFn])
  (:require [clojure.core.match :as match])

  (:require [clout.core :as clout])
  (:require [birdseye.core :refer
             [split-node-key
              dyn-segment-id
              dynamic-node-key?
              dynamic-node-key-seg?
              node-key-dyn-re]]))

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

(defn -url-generator [node-key params-map]
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

(defn -gen-url-matcher [sitemap & regexes]
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
          [:404
           {} ; this second val is the context map for the :404 node
           ]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tie it all together
(def default-404-response
  {:status 404
   :headers {"Content/Type" "text/html"}
   :body "Not Found"})

(def default-501-response
  {:status 501
   :headers {"Content/Type" "text/plain"}
   :body "Not Implemented"})

(def valid-http-methods-set
  #{:get :head :post :put :delete :options})

(defprotocol IUrlMapper
  (url-to-node [this url-path]) ; -> [node-key params-map]
  (node-to-url [this node-key params-map]))

(defprotocol ISiteNode
  (gen-url [this params])
  (get-breadcrumb [this params])

  (-handle-ring-request [this req])
  (-get-wrapped-handler-for-req [this req])
  (-get-ANY-method-handler [this req])
  (-get-501-handler [this req])
  (-get-handler-for-req [this req]))

(defprotocol IRingApp
  (-get-sitenode [this node-key])
  (-dispatch-request [this req]))

(defrecord SiteNode [node-key sitemap node-context-map ring-app]

  ISiteNode
  (gen-url [this params]
    (node-to-url ring-app node-key params))
  (get-breadcrumb [this params]
    (if-let [crumb (node-context-map :breadcrumb)]
      (if (fn? crumb)
        (crumb params)
        crumb)
      (str node-key)))

  (-handle-ring-request [this req]
    (let [req (assoc req
                :birdseye/node-key node-key
                :birdseye/sitemap sitemap
                :birdseye/node this)]
      ((-get-wrapped-handler-for-req this req) req)))

  (-get-wrapped-handler-for-req [this req]
    ;; this provides a way to do framework middleware, default
    ;; content-negotiation, etc.
    ;; TODO add support for contextual middleware on sub-sections of the
    ;; sitemap
    ;; TODO memoize this (memo key is the sitenode and the req params
    ;; used to find the handler, e.g. :http-method)
    (-get-handler-for-req this req))

  (-get-ANY-method-handler [this req]
    (or (node-context-map :any)
        (:birdseye/default-handler sitemap)))

  (-get-501-handler [this req]
    (or (:birdseye/handle-501 sitemap)
        (constantly default-501-response)))

  (-get-handler-for-req [this req]
    ;; 404's are handled elsewhere. This code is only reached for
    ;; valid urls.
    ;; TODO or lookup inherited default handler from parent node context
    (let [method (:http-method req)]
      (or (node-context-map method)
          (when (valid-http-methods-set method)
            (-get-ANY-method-handler this req))
          (-get-501-handler this req)))))

(defn set-default-handler [sitemap h]
  (assoc sitemap :birdseye/default-handler h))

(deftype UrlMapper [url-matcher url-generator]
  IUrlMapper
  (node-to-url [this node-key params-map]
    (url-generator node-key params-map))
  (url-to-node [this url-path] (url-matcher url-path)))

(deftype RingApp [sitemap sitenode-ctor url-mapper]
  ;; IFn support in order to provide: (ring-app req)
  IFn
  (invoke [this req] (-dispatch-request this req))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))

  IUrlMapper
  (node-to-url [this node-key params-map]
    (node-to-url url-mapper node-key params-map))
  (url-to-node [this url-path] (url-to-node url-mapper url-path))

  IRingApp
  (-dispatch-request [this req]
    (let [url-path (or (:path-info req) (:uri req))
          [node-key params] (url-to-node url-mapper url-path)
          sitenode (-get-sitenode this node-key)
          req (assoc req
                :path-params params
                :birdseye/url-path url-path)]
      (-handle-ring-request sitenode req)))

  (-get-sitenode [this node-key]
    ;; cache these
    (sitenode-ctor {:node-key node-key
                    :sitemap sitemap
                    :node-context-map (sitemap node-key)
                    :ring-app this})))

(defn gen-ring-app [sitemap]
  (let [sitemap (if (:404 sitemap)
                  sitemap
                  (assoc sitemap :404 {:any (constantly
                                             default-404-response)}))
        url-mapper (UrlMapper. (-gen-url-matcher sitemap) -url-generator)]
    (RingApp. sitemap map->SiteNode url-mapper)))
