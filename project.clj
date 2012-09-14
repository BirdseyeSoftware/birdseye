(defproject birdseye "1.0.0-SNAPSHOT"
  :description "A sitemap/url-routing system for Clojure web apps."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.match "0.2.0-alpha10"]
                 [ring "1.1.5"]
                 [clout "1.1.0"]
                 ]
  :exclusions [clj-stacktrace]
  :dev-dependencies [[ring-mock "0.1.3"]])
