(ns birdseye.macros
  (:require [birdseye.sitemap]))

(defmacro gen-sitemap [mapforms]
  `(birdseye.sitemap/-gen-sitemap ~(birdseye.sitemap/-normalize-map-forms mapforms)))

(defmacro defsitemap [name mapforms-vec & body]
  `(do
     (def ~name
       (-> (gen-sitemap ~mapforms-vec)
           ~@body))))
