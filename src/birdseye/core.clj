(ns birdseye.core)

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
