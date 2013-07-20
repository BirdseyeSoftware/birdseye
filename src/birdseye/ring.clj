(ns birdseye.ring
  (:require [clojure.string :as string]
            [birdseye.sitemap :refer [lookup-context-in-hierarchy]]
            [birdseye.url-mapping :refer
             [gen-default-url-mapper
              IUrlMapper
              node-to-url
              url-to-node]])
  (:import [clojure.lang IFn]))

(defprotocol ISiteNode
  (gen-url [this params])
  (get-breadcrumb [this params])
  (lookup-context [this key])

  (-handle-ring-request [this req])
  (-get-wrapped-handler-for-req [this req])
  (-get-ANY-method-handler [this req])
  (-get-handler-for-req [this req]))

(defprotocol IRingApp
  (-get-sitenode [this node-key])
  (-dispatch-request [this req]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-404-response
  {:status 404
   :headers {"content-type" "text/html"}
   :body "Not Found"})

(def default-501-response
  {:status 501
   :headers {"content-type" "text/html"}
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

  (lookup-context [this key]
    (lookup-context-in-hierarchy sitemap node-key key))

  (-handle-ring-request [this req]
    (let [req (assoc req
                :birdseye/node this
                :birdseye/node-key node-key
                :birdseye/node-context node-context-map
                :birdseye/sitemap sitemap)
          handler (-get-wrapped-handler-for-req this req)]
      (try (handler req)
           (catch Exception e
             (if-let [exc-handler (lookup-context this :birdseye/http-500)]
               (exc-handler (assoc req :birdseye/exception e))
               (throw e))))))

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
        (lookup-context this :birdseye/default-handler)))

  (-get-handler-for-req [this req]
    ;; This code is only reached for valid urls.
    ;; 404's are handled by routing to node-key :birdseye/http-404.
    (let [method (:request-method req)]
      (or (node-context-map method)
          (when (valid-http-methods-set method)
            (-get-ANY-method-handler this req))
          (lookup-context this :birdseye/http-501)))))

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
  (update-in
   sitemap
   [:birdseye/root-context :birdseye/default-handler]
   (constantly h)))

(defn default-500-handler [req]
  {:status 500
   :headers {"content-type" "text/html"}
   :body "An unexpected server error occurred."})

(def root-context {:birdseye/http-501 (constantly default-501-response)
                   :birdseye/http-500 default-500-handler})

(defn normalize-sitemap [sitemap]
  (let [mk-any-handler (fn [resp] {:any (constantly resp)})]
    (-> sitemap
        (update-in [:birdseye/root-context] #(merge root-context %))
        (update-in [:birdseye/http-404]
                   #(or % (mk-any-handler default-404-response))))))

(defn gen-ring-app [sitemap]
  (map->RingApp
   {:sitemap (normalize-sitemap sitemap)
    :sitenode-ctor map->SiteNode
    :url-mapper (gen-default-url-mapper sitemap)}))
