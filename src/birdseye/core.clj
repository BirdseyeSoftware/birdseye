(ns birdseye.core
  (:refer-clojure :exclude [sym])
  (:require [clojure.string :as string])

  (:require [clojure.core.match :as match]))

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
       Parent node '%s' does not exist. %s"
       node-key parent-key (keys site-map)))))

(defn- validate-site-map-addition [site-map index-in-forms node-key context-map]
  (if (not (keyword? node-key))
    (throwf (str "Was expecting a site-map node-key"
                 " in defsitemap position %s") index-in-forms))
  (if (= \. (last (name node-key)))
    (throwf "Node keys must not end in a dot: %s" node-key))
  (if (site-map node-key)
    (throwf "%s is already in the site-map." node-key))
  (assert-parent-node-exists node-key site-map)
  (if (not (or (map? context-map)
               (nil? context-map)))
    (throwf "Was expecting a site-map node context map, i.e. a hash-map
          not a %s." (type context-map))))

(defn any-form-of-nil? [v]
  (or (nil? v) (#{:nil 'nil} v)))

(defn match-forms [forms]
  (match/match [forms]
    [([(nil-as-k :when any-form-of-nil?) & r] :seq)]
    {:error :nil :message "nil is not a valid node-key"}

    [([(k :when false?) & r] :seq)]
    {:error :false :message "false is not a valid node-key"}

    [([(k :when keyword?) (next-k :when keyword?) & r] :seq)]
    {:node-key k}

    [([(k :when keyword?) (children :when vector?) & r] :seq)]
    {:node-key k :children children}

    [([(k :when keyword?)
       (context-map :when map?)
       (children :when vector?) & r] :seq)]
    {:node-key k :context-map context-map :children children}

    [([(k :when keyword?) (context-map :when map?) & r] :seq)]
    {:node-key k :context-map context-map}

    [([(k :when keyword?) & r] :seq)]
    {:node-key k}

    [_]
    {:error (first forms)}))

(defn gen-sitemap [mapforms]
  (let [n (count mapforms)]
    (loop [i 0
           site-map {}]
      (if (< i n)
        (let [rest-forms (drop i mapforms)
              match (match-forms rest-forms)]
          (let [{:keys [node-key children context-map error]
                 :or {context-map {}}} match]
            (if error
              (throwf
               "Invalid site-map entry at position %s: %s" i (:error match)))
            (validate-site-map-addition site-map i node-key context-map)
            (recur (+ i (count match)) (assoc site-map node-key context-map))))
        site-map))))

(defn normalize-map-forms [mapforms & prefix]
  (let [prefix (and prefix (first prefix))
        normalize-key (fn [k-name]
                        (if (and prefix
                                 (= \. (first k-name)))
                          (str prefix k-name)
                          k-name))]
    (for [form mapforms]
      (cond
        (named? form)
        (keyword (normalize-key (name form)))
        ;; (vector? form) (normalize-map-forms form)
        :else
        form)))
  )

(defmacro defsitemap [& mapforms]
  `(gen-sitemap (vector ~@(normalize-map-forms mapforms))))


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
