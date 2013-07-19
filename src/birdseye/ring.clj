(ns birdseye.ring
  (:require [clojure.string :as string]
            [birdseye.url-mapping :refer
             [gen-default-url-mapper
              IUrlMapper
              node-to-url
              url-to-node]])
  (:import [clojure.lang IFn]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;; used to find the handler, e.g. :request-method)
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
    (let [method (:request-method req)]
      (or (node-context-map method)
          (when (valid-http-methods-set method)
            (-get-ANY-method-handler this req))
          (-get-501-handler this req)))))

(defrecord RingApp [sitemap sitenode-ctor url-mapper]
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
    ;; cache these?
    (sitenode-ctor {:node-key node-key
                    :sitemap sitemap
                    :node-context-map (sitemap node-key)
                    :ring-app this})))

(defn set-default-handler [sitemap h]
  (assoc sitemap :birdseye/default-handler h))

(defn add-error-handlers [sitemap]
  (if (:http-404 sitemap)
    sitemap
    (assoc sitemap :http-404 {:any (constantly
                               default-404-response)})))

(defn gen-ring-app [sitemap]
  (map->RingApp
   {:sitemap (add-error-handlers sitemap)
    :sitenode-ctor map->SiteNode
    :url-mapper (gen-default-url-mapper sitemap)}))
